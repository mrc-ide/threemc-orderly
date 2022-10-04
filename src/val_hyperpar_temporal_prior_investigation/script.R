#### Testing temporal priors & time-related hyperparameters for OOS fit ####

#### libs ####
# devtools::load_all("../threemc")
# remotes::install_github("mrc-ide/threemc")
# library(threemc)
# library(dplyr)
# library(ggplot2)
# library(readr)
# library(sf)

# source("code/Plots/0_source_plots.R")
source("source.R")

#### Metadata to run the models ####

# remove circumcisions with missing type?
rm_missing_type <- FALSE

# set country
# cntry <- "MWI"

k_dt <- 5 # Age knot spacing
start_year <-  2002
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
N <- 1000
forecast_year <- 2021
# paed_age_cutoff <- NULL
paed_age_cutoff <- 10
area_lev <- 0 # run at national level
CI_range <- 0.95

# five-year age groups
spec_age_groups <- five_year_age_groups <- c(
  "0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "54-59"
)

# dependencies and save locations
dir_path <- "depends/"
save_loc <- "artefacts/"
threemc::create_dirs_r(save_loc)

#### Initial Model Setup ####

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# read in data, filter for specific country and male surveys only
filters <- c("iso3" = cntry, sex = "male")
areas <- read_circ_data(paste0(dir_path, "areas.geojson"), filters) %>%
  dplyr::mutate(space = 1:dplyr::n()) # add space column to areas
survey_circumcision <- read_circ_data(
  paste0(dir_path, "survey_circumcision.csv.gz"), filters
)
populations <- read_circ_data(
  paste0(dir_path, "population_singleage_aggr.csv.gz"),
  filters
)

# load hyperpars for countries with largest uncertainty bounds
optimal_hyperpars <- readr::read_csv(
  paste0(dir_path, "high_var_hyperpars.csv.gz")
  ) %>%
  select(-iso3) %>%
  summarise(across(everything(), median)) %>%
  unlist()
optimal_hyperpars <- round(optimal_hyperpars, 9) # ensure hyperparams are known

# load empirical survey estimates
survey_estimate <- readr::read_csv(
  paste0(dir_path, "survey-circumcision-coverage.csv.gz")
  ) %>%
  filter(iso3 == cntry) %>%
  survey_points_dmppt2_convert_convention() %>%
  rename(indicator = type) %>%
  filter(indicator == "MMC coverage") # only want to look at MMC coverage

# load all surveys
all_survey_df <- readr::read_csv(paste0(dir_path, "all_surveys.csv")) %>%
  filter(area_id == cntry)

# years for removed surveys from OOS validation
survey_years <- unique(all_survey_df$survey_year)
if (length(survey_years) < 2) stop("Too few survey years")
second_last_year <- survey_years[length(survey_years) - 1]
if (last(survey_years) - second_last_year == 1) {
  removed_years <- c(second_last_year, last(survey_years))
} else {
  removed_years <- last(survey_years)
}

# remove latest survey years
survey_circumcision <- survey_circumcision %>%
  mutate(year = as.numeric(substr(survey_id, 4, 7))) %>%
  filter(!year %in% removed_years)

# pull latest and first censoring year from survey_id
survey_years <- as.numeric(substr(unique(survey_circumcision$survey_id), 4, 7))

cens_year <- max(survey_years)
start_year <- min(c(survey_years - 2, start_year)) # have lower bound on start

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circ_preprocess <- prepare_survey_data(
  areas               = areas,
  survey_circumcision = survey_circumcision,
  area_lev            = area_lev,
  start_year          = start_year,
  cens_year           = cens_year,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
)

if (nrow(survey_circ_preprocess) == 0) {
  message("no valid surveys at this level") # move inside function!
}

# include indicator to determine whether there is any type distinction for cntry
if (all(is.na(survey_circ_preprocess$circ_who) &
        is.na(survey_circ_preprocess$circ_where))) {
  # print("No type distinction made in valid surveys for this country")
  stop("No type distinction made in valid surveys for this country")
} 
is_type <- TRUE

# Skeleton dataset
out <- create_shell_dataset(
  survey_circumcision = survey_circ_preprocess,
  populations         = populations,
  areas               = areas,
  area_lev            = area_lev,
  start_year          = start_year,
  end_year            = forecast_year,
  time1               = "time1",
  time2               = "time2",
  strat               = "space",
  age                 = "age",
  circ                = "indweight_st"
)

#### ? ####

# Plan: fit for AR 1 first, then functionalise so you won't repeat for RW1/RW2

dat_tmb <- threemc_prepare_model_data(
  out               = out,
  areas             = areas,
  area_lev          = area_lev,
  aggregated        = TRUE,
  weight            = "population",
  k_dt              = k_dt,
  rw_order          = NULL,
  paed_age_cutoff   = paed_age_cutoff
)

# specify TMB model
if (is_type == TRUE & is.null(paed_age_cutoff)) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
} else if (is_type == TRUE) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC"
} else {
  mod <- "Surv_SpaceAgeTime"
}

# remove mmc time correlation parameters, if fitting with RW precision matrix
if (!is.null(dat_tmb$Q_time)) {
  mod <- paste0(mod, "_RW")
}

if (is.null(paed_age_cutoff)) {
  X_fixed_mmc_paed <- X_age_mmc_paed <- X_space_mmc_paed <- data.frame(0)
}

set_params <- function(mod, dat_tmb) {
  # Initial values
  parameters <- with(
    dat_tmb,
    list(
      # intercept
      "u_fixed_mmc"            = rep(-5, ncol(X_fixed_mmc)),
      "u_fixed_mmc_paed"       = rep(-5, ncol(X_fixed_mmc_paed)),
      "u_fixed_tmc"            = rep(-5, ncol(X_fixed_tmc)),
      # age random effect
      "u_age_mmc"              = rep(0, ncol(X_age_mmc)),
      "u_age_mmc_paed"         = rep(0, ncol(X_age_mmc_paed)),
      "u_age_tmc"              = rep(0, ncol(X_age_tmc)),
      # time random effect for (non-paed) MMC
      "u_time_mmc"             = rep(0, ncol(X_time_mmc)),
      # time random effect for TMC
      # "u_time_tmc"             = rep(0, ncol(X_time_tmc)),
      # Space random effect (district)
      "u_space_mmc"            = rep(0, ncol(X_space_mmc)),
      "u_space_mmc_paed"       = rep(0, ncol(X_space_mmc_paed)),
      "u_space_tmc"            = rep(0, ncol(X_space_tmc)),
      # Interactions for MMC
      "u_agetime_mmc"          = matrix(0, ncol(X_age_mmc), ncol(X_time_mmc)),
      "u_agespace_mmc"         = matrix(0, ncol(X_age_mmc), ncol(X_space_mmc)),
      "u_spacetime_mmc"        = matrix(0, ncol(X_time_mmc), ncol(X_space_mmc)),
      "u_agespace_mmc_paed"    = matrix(0, ncol(X_age_mmc_paed), ncol(X_space_mmc_paed)),
      # Interactions for TMC
      "u_agespace_tmc"         = matrix(0, ncol(X_age_tmc), ncol(X_space_tmc)),
      # Autocorrelation parameters for priors
      # Variance
      "logsigma_age_mmc"            = 0,
      "logsigma_age_mmc_paed"       = 0,
      "logsigma_time_mmc"           = 0,
      "logsigma_space_mmc"          = 0,
      "logsigma_space_mmc_paed"     = 0,
      "logsigma_agetime_mmc"        = 0,
      "logsigma_agespace_mmc"       = 0,
      "logsigma_agespace_mmc_paed"  = 0,
      "logsigma_spacetime_mmc"      = 0,
      "logsigma_age_tmc"            = 0,
      # "logsigma_time_tmc"           = 0,
      "logsigma_space_tmc"          = 0,
      "logsigma_agespace_tmc"       = 0,
      # Mean
      "logitrho_mmc_time1"          = 2,
      "logitrho_mmc_time2"          = 2,
      "logitrho_mmc_time3"          = 2,
      "logitrho_mmc_age1"           = 2,
      "logitrho_mmc_paed_age1"      = 2,
      "logitrho_mmc_age2"           = 2,
      "logitrho_mmc_paed_age2"      = 2,
      "logitrho_mmc_age3"           = 2,
      # "logitrho_tmc_time1"          = 2,
      "logitrho_tmc_age1"           = 2,
      "logitrho_tmc_age2"           = 2
    )
  )
  
  # remove paed-related parameters if not desired
  if (is.null(paed_age_cutoff)) {
    # remove paed-related parameters
    parameters <- parameters[!grepl("paed", names(parameters))]
  }
  
  # remove mmc time correlation parameters, if fitting with RW precision matrix
  if ("Q_time" %in% names(dat_tmb)) {
    parameters <- parameters[!grepl("logitrho_mmc_time", names(parameters))]
  }
  return(parameters)
}
parameters <- set_params(mod, dat_tmb)

#### Functions for incrementing hyperparameters and drawing comparisons ####

# increment fixed values for time related mmc hyperparameters
increment_hyperpars <- function(parameters, optimal_hyperpars, fit, increment = 1) {
  for (i in seq_along(optimal_hyperpars)) {
    current_par_val <- as.vector(fit$par[names(optimal_hyperpars)[i]])
    # in case parameter is fixed already, won't be in fit$par then
    if (is.na(current_par_val)) {
      current_par_val <- parameters[[names(optimal_hyperpars)[i]]]
    }
    if (optimal_hyperpars[i] > 0) {
      if (parameters[names(optimal_hyperpars)[i]] <= 0) {
        parameters[names(optimal_hyperpars)[i]] <- increment
      } else {
        parameters[names(optimal_hyperpars)[i]] <-
          current_par_val + increment
      }
    } else {
      if (parameters[names(optimal_hyperpars)[i]] >= 0) {
        parameters[names(optimal_hyperpars)[i]] <- -increment
      } else {
        parameters[names(optimal_hyperpars)[i]] <-
          current_par_val - increment
      }
    }
  }
  return(parameters)
}

# function to aggregate age groups and pivot estimates longer
out_wide_to_long <- function(.data, spec_age_groups) {
  .data <- .data %>%
    select(area_id:age, contains("cum_inc")) %>%
    tidyr::pivot_longer(
      # all_of(c("cum_inc_mmcM", "cum_incM", "cum_inc_tmcM")),
      all_of(c("cum_inc_mmcM")),
      names_to = "type",
      values_to = "mean"
    ) %>%
    tidyr::pivot_longer(
      # all_of(c("cum_inc_mmcL", "cum_incL", "cum_inc_tmcL")),
      all_of(c("cum_inc_mmcL")),
      names_to = "type_lower",
      values_to = "lower"
    ) %>%
    tidyr::pivot_longer(
      # all_of(c("cum_inc_mmcU", "cum_incU", "cum_inc_tmcU")),
      all_of(c("cum_inc_mmcU")),
      names_to = "type_upper",
      values_to = "upper"
    ) %>%
    mutate(
      across(contains("type"), ~ stringr::str_remove(., c("M|L|U")))
    ) %>%
    filter(type == type_lower, type == type_upper) %>%
    select(-c(type_lower, type_upper)) %>%
    mutate(
      type = case_when(
        grepl("tmc", type) ~ "TMC coverage",
        grepl("mmc", type) ~ "MMC coverage",
        TRUE               ~ "MC coverage"
      )
    ) %>%
    left_join(select(populations, area_id, year, age, population)) %>%
    threemc:::aggregate_sample_age_group(
      aggr_cols = c("area_id", "area_name", "area_level", "year", "type"),
      num_cols = c("mean", "lower", "upper"),
      age_groups = spec_age_groups
    ) %>%
    filter(!is.na(age_group))
  return(.data)
}

# compare models based on ppc
# Idea: If we make error bounds "worse", reject proposal model
# Else, we must also not hurt our models mean behaviour, as tested with CRPS and ELPD
threemc_ppc_compare <- function(ppc_orig, ppc_proposal) {
  
  # tabulate ELPD comparison
  loo_tbl <- loo::loo_compare(
    ppc_orig$summary_stats$elpd, ppc_proposal$summary_stats$elpd
  )
  print(loo_tbl, simplify = FALSE, digits = 3)
  
  orig_message <- "Original model preferable"
  prop_message <- "Proposal model preferable"
  orig_return <- list("optimal_model" = "original", "loo_tbl" = loo_tbl)
  prop_return <- list("optimal_model" = "proposal", "loo_tbl" = loo_tbl)
  
  # check based on CI
  ci_orig <- ppc_orig$summary_stats$oos_observations_within_PPD_CI
  ci_prop <- ppc_proposal$summary_stats$oos_observations_within_PPD_CI
  
  if (ci_orig >= ci_prop) {
    message(orig_message)
    return(orig_return)
  } else {
    # message(prop_message)
    # return(prop_return)
    
    # check based on CRPS (aggregate over all, lower the better)
    crps_diff <- sum(ppc_orig$summary_stats$crps) -
      sum(ppc_proposal$summary_stats$crps)
    if (crps_diff < 0) {
      message(orig_message)
      return(orig_return)
    } else if (crps_diff == 0) {
      message(prop_message)
      return(prop_return)
    }
    
    # check based on elpd
    if (loo_tbl[rownames(loo_tbl) == "model2", "elpd_diff"] < 0) {
      message(orig_message)
      return(orig_return)
    } else if (loo_tbl[rownames(loo_tbl) == "model1", "elpd_diff"] < 0) {
      message(prop_message)
      return(prop_return)
    }
    
    # finally, if all other methods have been exhausted
    message(paste(
      "Could not make a decision!",
      "Going with original model based on parsimony"
    ))
    return(orig_return)
  }
}

# fit model with fixed hyperpars
fit_proposal_model <- function(proposal_parameters, maps) {
  
  # fit proposal TMB model
  set.seed(123)
  fit_fixed <- threemc_fit_model(
    dat_tmb    = dat_tmb,
    mod        = mod,
    parameters = proposal_parameters,
    maps       = maps,
    randoms    = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc"
    ),
    N          = N
  )
  
  # subset to specific area level and calculate quantiles for rates and hazard
  out_spec_fixed <- compute_quantiles(out, fit_fixed, area_lev = area_lev) %>%
    # prepare for output
    select(
      area_id, area_name, area_level, year, age = circ_age,
      contains("cum_inc_mmc")
    )
  
  # perform posterior predictive checks on model
  ppc_fixed <- threemc_oos_ppc(
    fit_fixed,
    out_spec_fixed,
    populations,
    survey_estimate,
    removed_years,
    type = "coverage",
    age_groups = spec_age_groups,
    CI_range = CI_range,
    compare_stats = TRUE
  )
  return(list("fit" = fit_fixed,
              "out" = out_spec_fixed,
              "ppc" = ppc_fixed))
}

#### Main Function for Comparison ####

# function to perform increments to model estimates until "best" model is found
# Want to pull in above to make this!
threemc_optimise_model <- function(
    out,
    dat_tmb,
    mod,
    parameters,
    survey_estimates,
    removed_years,
    spec_age_groups = c("15-29", "30-49"),
    CI_range = 0.95,
    optimal_hyperpars,
    increment = 1,
    add_title = NULL
) {
  
  # Plan:
  # Want to increment hyperparameters to determine the best model
  # (1) Specify original model
  # Function
  
  # Also want to tabulate ppc stats for each model
  
  # So want to return:
  # 1. Optimal model (out_opt)
  # 2. Tabulated comparisons of all models (include increment beside it)
  # 3. Plots comparing each proposed model step
  
  #### PPD for original model ####
  
  # fit initial TMB model
  set.seed(123)
  fit <- threemc_fit_model(
    dat_tmb    = dat_tmb,
    mod        = mod,
    parameters = parameters,
    randoms    = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc"
    ),
    N          = N
  )
  
  # subset to specific area level and calculate quantiles for rates and hazard
  out_spec <- compute_quantiles(out, fit, area_lev = area_lev) %>%
    # prepare for output
    select(
      area_id, area_name, area_level, year, age = circ_age,
      contains("cum_inc_mmc")
    )
  
  # perform posterior predictive checks on model
  ppc <- threemc_oos_ppc(
    fit,
    out_spec,
    populations,
    survey_estimate,
    removed_years,
    type = "coverage",
    age_groups = spec_age_groups,
    CI_range = CI_range,
    compare_stats = TRUE
  )
  
  
  #### Fit additional model with fixed params and calculate PPD ####
  
  # increment fixed values for time related mmc hyperparameters
  proposal_parameters <- increment_hyperpars(
    parameters, optimal_hyperpars, fit, increment = increment
  )
  
  # hold MMC time variance hyperparameters fixed!!
  maps <- lapply(rep(NA, length(optimal_hyperpars)), factor)
  names(maps) <- names(optimal_hyperpars)
  
  # fit model with fixed hyperpars
  set.seed(123)
  fixed_mod <- fit_proposal_model(proposal_parameters, maps)
  
  
  #### Compare Models ####
  
  # Compare two model fits by plotting coverage vs year and age group for both
  # TODO: Rotate xlab in threemc_val_plt
  year_plots <- age_group_plots <- list()
  plots <- list("year_plots" = year_plots, "age_group_plots" = age_group_plots)
  
  # function to update plots
  update_plots <- function(
    out_1,
    out_2,
    plots,
    spec_age_groups = c("15-29", "30-49"),
    add_title = NULL
  ) {
    
    year_plots <- plots$year_plots
    age_group_plots <- plots$age_group_plots
    
    title <- "Hyperparameter Comparison, "
    if (!is.null(add_title)) title <- paste0(title, add_title)
    
    out_spec_1 <- out_wide_to_long(out_1, spec_age_groups)
    out_spec_2 <- out_wide_to_long(out_2, spec_age_groups)
    year_plots[[length(year_plots) + 1]] <- threemc_val_plt(
      df_results_oos = bind_rows(
        mutate(out_spec_1, indicator = "Current"),
        mutate(out_spec_2, indicator = "Proposal")
      ),
      df_results_survey = rename(survey_estimate, type = indicator),
      all_surveys = filter(all_survey_df, !survey_year %in% removed_years),
      spec_agegroup = spec_age_groups,
      spec_type = "MMC coverage",
      xlab = "Year",
      ylab = "Circumcision Coverage",
      title = title
    )
    
    out_spec_1_5_year <- out_wide_to_long(out_1, five_year_age_groups)
    out_spec_2_5_year <- out_wide_to_long(out_2, five_year_age_groups)
    age_group_plots[[length(age_group_plots) + 1]] <- threemc_val_plt(
      df_results_oos = bind_rows(
        mutate(out_spec_1_5_year, indicator = "Current"),
        mutate(out_spec_2_5_year, indicator = "Proposal")
      ),
      # df_results_survey = rename(survey_estimate, indicator = type),
      df_results_survey = rename(survey_estimate, type = indicator),
      all_surveys = filter(all_survey_df, !survey_year %in% removed_years),
      spec_agegroup = five_year_age_groups,
      spec_years = removed_years,
      x_var = "age_group",
      spec_type = "MMC coverage",
      xlab = "Five Year Age Group",
      ylab = "Circumcision Coverage",
      title = title
    )
    
    return(list("year_plots" = year_plots, "age_group_plots" = age_group_plots))
  }
  
  plots <- update_plots(
    out_spec, fixed_mod$out, plots, add_title = add_title
  )
  
  # Compare with PPCs
  choice <-  threemc_ppc_compare(ppc, fixed_mod$ppc)
  
  # tabulate fits
  fit_tab <- data.frame()
  
  # function to update fit table
  update_fit_tab <- function(fit_tab, ppc, mod_label) {
    fit_tab_update <- tribble(
      ~model,     ~ci, ~elpd, ~crps,
      mod_label, NA,   NA,   NA
    )
    fit_tab_update$ci <- ppc$summary_stats$oos_observations_within_PPD_CI
    fit_tab_update$elpd <- ppc$summary_stats$elpd$estimates[1, 1]
    fit_tab_update$crps <- sum(ppc$summary_stats$crps)
    return(bind_rows(fit_tab, fit_tab_update))
  }
  fit_tab <- update_fit_tab(fit_tab, ppc, "original")
  fit_tab <- update_fit_tab(fit_tab, fixed_mod$ppc, "proposal")
  
  
  #### Continue to Increment & Choose Model ####
  
  # if original model was preferred, try reducing increment on parameters
  increment_orig <- increment
  # if (choice$optimal_model == "original") {
    # TEMP: always take proposal model for now
  #   increment <- increment / 2
  #   current_model <- list("fit" = fit, "out" = out_spec, "ppc" = ppc)
  #   current_params <- parameters
  # } else {
    current_model <- fixed_mod
    current_params <- proposal_parameters
  # }
  
  count <- 2
  
  # TEMP: save all parameters
  pars_list_to_df <- function(parameters, names) {
    t(unstack(stack(parameters[names])))
  }
  # parameters from original and fixed model
  parameters_df <- rbind(
    pars_list_to_df(fit$par, names(maps)),
    pars_list_to_df(proposal_parameters, names(maps))
  )
  
  # give parameter increment two chances to improve fit
  # while (increment >= increment_orig / 2) {
  while (count <= 5) {
    proposal_pars <- increment_hyperpars(
      current_params,
      optimal_hyperpars,
      current_model$fit,
      increment = increment
    )
    
    # TODO: Add tryCatch here
    # prop_mod <- fit_proposal_model(proposal_pars)
    prop_mod <- tryCatch({
      fit_proposal_model(proposal_pars, maps)
    }, error = function(cond) {
      message("Proposal model failed to fit")
      message("Original error message:")
      message(cond)
      return(NA)
    })
    if (inherits(fixed_mod, "logical") && is.na(prop_mod)) break
    
    # update table of parameters
    parameters_df <- rbind(
      parameters_df,
      pars_list_to_df(proposal_pars, names(maps))
    )
    
    plots <- update_plots(
      current_model$out, prop_mod$out, plots, add_title = add_title
    )
    
    # Compare with PPCs
    choice <-  threemc_ppc_compare(current_model$ppc, prop_mod$ppc)
    
    # tabulate fits
    fit_tab <- update_fit_tab(
      fit_tab,
      prop_mod$ppc,
      paste0("proposal ", count, " (increment: ", increment, ")")
    )
    
    # TEMP: Accept all proposals
    # if (choice$optimal_model == "original") {
    #   increment <- increment / 2
    # } else {
    current_model <- prop_mod
    current_params <- proposal_pars
    # }
    count <- count + 1
    gc()
  }
  
  # fit model with fixed hyperpars
  return(list(
    "optimal_parameters" = current_params,
    "parameters_df" = dplyr::as_tibble(parameters_df), # temp output
    # "mod" = current_model,
    "plots" = plots,
    "fit_tab" = fit_tab
  ))
}

#### Run for model with AR 1 temporal prior ####

ar_fits <- threemc_optimise_model(
  out,
  dat_tmb,
  mod,
  parameters,
  survey_estimates,
  removed_years,
  spec_age_groups,
  CI_range,
  optimal_hyperpars,
  increment = 2,
  add_title = "AR 1 prior, "
)

#### Run for RW 1 models ####

#### RW 1 ####

dat_tmb <- threemc_prepare_model_data(
  out               = out,
  areas             = areas,
  area_lev          = area_lev,
  aggregated        = TRUE,
  weight            = "population",
  k_dt              = k_dt,
  rw_order          = 1,
  paed_age_cutoff   = paed_age_cutoff
)

# specify TMB model
if (is_type == TRUE & is.null(paed_age_cutoff)) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
} else if (is_type == TRUE) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC"
} else {
  mod <- "Surv_SpaceAgeTime"
}

# remove mmc time correlation parameters, if fitting with RW precision matrix
if (!is.null(dat_tmb$Q_time)) {
  mod <- paste0(mod, "_RW")
}

parameters <- set_params(mod, dat_tmb)

rw_1_fits <- threemc_optimise_model(
  out,
  dat_tmb,
  mod,
  parameters,
  survey_estimates,
  removed_years,
  spec_age_groups,
  CI_range,
  # need to remove logitrho hyperparameters when using RW temporal prior
  optimal_hyperpars[names(optimal_hyperpars) %in% names(parameters)],
  increment = 2,
  add_title = "RW 1 prior, "
)


#### RW 2 ####

dat_tmb <- threemc_prepare_model_data(
  out               = out,
  areas             = areas,
  area_lev          = area_lev,
  aggregated        = TRUE,
  weight            = "population",
  k_dt              = k_dt,
  rw_order          = 2,
  paed_age_cutoff   = paed_age_cutoff
)

parameters <- set_params(mod, dat_tmb)

rw_2_fits <- threemc_optimise_model(
  out,
  dat_tmb,
  mod,
  parameters,
  survey_estimates,
  removed_years,
  spec_age_groups,
  CI_range,
  optimal_hyperpars[names(optimal_hyperpars) %in% names(parameters)],
  increment = 2,
  add_title = "RW 2 prior, "
)

#### Save Output ####

# save parameters from each run
parameters_df <- bind_rows(
  mutate(ar_fits$parameters_df,   prior = "AR 1"),
  mutate(rw_1_fits$parameters_df, prior = "RW 1"),
  mutate(rw_2_fits$parameters_df, prior = "RW 2"),
)

readr::write_csv(
  parameters_df,
  file = paste0(save_loc, "hyperparameters.csv")
)

# save ppcs from each run
fit_tab <- bind_rows(
  mutate(ar_fits$fit_tab,   prior = "AR 1"),
  mutate(rw_1_fits$fit_tab, prior = "RW 1"),
  mutate(rw_2_fits$fit_tab, prior = "RW 2"),
)

readr::write_csv(
  fit_tab,
  file = paste0(save_loc, "fit_stats.csv")
)

# save plots

# save_plots <- function(plots, name) {
#     pdf(name)
#     plots
#     dev.off()
# }
#
# save_plots(ar_fits$plots$year_plots, paste0(cntry, "_ar1_year_plots.pdf"))
# save_plots(ar_fits$plots$age_group_plots, paste0(cntry, "_ar1_agegroup_plots.pdf"))
# save_plots(rw_1_fits$plots$year_plots, paste0(cntry, "_rw1_year_plots.pdf"))
# save_plots(rw_1_fits$plots$age_group_plots, paste0(cntry, "_rw1_agegroup_plots.pdf"))
# save_plots(rw_2_fits$plots$year_plots, paste0(cntry, "_rw2_year_plots.pdf"))
# save_plots(rw_2_fits$plots$age_group_plots, paste0(cntry, "_rw2_agegroup_plots.pdf"))

pdf(paste0(save_loc, "ar1_year_plots.pdf"))
ar_fits$plots$year_plots
dev.off()

pdf(paste0(save_loc, "ar1_agegroups_plots.pdf"))
ar_fits$plots$age_group_plots
dev.off()

pdf(paste0(save_loc, "rw1_year_plots.pdf"))
rw_1_fits$plots$year_plots
dev.off()

pdf(paste0(save_loc, "rw1_agegroups_plots.pdf"))
rw_1_fits$plots$age_group_plots
dev.off()

pdf(paste0(save_loc, "rw2_year_plots.pdf"))
rw_2_fits$plots$year_plots
dev.off()

pdf(paste0(save_loc, "rw2_agegroups_plots.pdf"))
rw_2_fits$plots$age_group_plots
dev.off()
