#### Initial ####

### Metadata to run the models

# !! Change this to use dataset stored in threemc
k_dt_age <- 5 # Age knot spacing
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
N <- 1000
forecast_year <- 2021
start_year <- 1998
# paed_age_cutoff <- 10
# five-year age groups to perform posterior predictive checks for
five_year_age_groups <- c(
  "0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "54-59"
)
CI_range <- c(0.5, 0.8, 0.95) # Confidence intervals to find for PPD 

# reset parameters
if (!is.numeric(paed_age_cutoff) || is.infinite(paed_age_cutoff)) {
  paed_age_cutoff <- NULL
}
print(paste("paed_age_cutoff is", paed_age_cutoff))

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

if (!rw_order %in% c(1, 2)) {
  message("rw_order not 1 or 2, assuming AR 1 model")
  rw_order <- NULL
}

# inc_time_tmc <- TRUE 
# vmmc_countries <- c(
#   "LSO", "MOZ", "NAM", "RWA", "SWZ", "TZA", 
#   "UGA", "ZWE", "ZMB", "KEN", "ETH"
# )
# if (cntry %in% vmmc_countries[!vmmc_countries %in% c("ETH", "KEN")]) {
#   inc_time_tmc <- FALSE 
# }

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

# read in data, filter for specific country and male surveys only
filters <- c("iso3" = cntry, sex = "male")
areas <- read_circ_data("depends/areas.geojson", filters) %>% 
  dplyr::mutate(space = 1:dplyr::n()) # add space column to areas
areas <- st_make_valid(areas) 
survey_circumcision <- read_circ_data(
  "depends/survey_circumcision.csv.gz", 
  filters
  ) %>% 
  mutate(survey_year = as.numeric(substr(survey_id, 4, 7)))
populations <- read_circ_data("depends/population_singleage_aggr.csv.gz", filters)

# pull recommended area hierarchy for target country
area_lev <- threemc::datapack_psnu_area_level %>%
  filter(iso3 == cntry) %>%
  pull(psnu_area_level)

# don't model at the country level
if (length(area_lev) > 0 && area_lev == 0) area_lev <- NULL 

# if area_level is missing, assume most common area lev in surveys
if (length(area_lev) == 0) {
  area_lev <- table(as.numeric(substr(survey_circumcision$area_id, 5, 5)))
  area_lev <- as.numeric(names(area_lev)[area_lev == max(area_lev)])
}
# area_lev <- 0 # run at national level

# remove most recent survey 
# if most recent surveys are one year apart, remove both
survey_years <- unique(survey_circumcision$survey_year)
if (length(survey_years) < 2) stop("Too few survey years")
second_last_year <- survey_years[length(survey_years) - 1]
if (last(survey_years) - second_last_year == 1) {
  removed_years <- c(second_last_year, last(survey_years))
} else {
  removed_years <- last(survey_years)
}

# survey removed from training data
survey_circumcision_test <- survey_circumcision %>% 
  filter(survey_year %in% removed_years)

# surveys in training data (removing test surveys)
survey_circumcision <- survey_circumcision %>% 
  filter(!survey_year %in% removed_years)


# re-calculate start as earliest year - 60 (only where TMC needs to vary)
survey_years <- unique(survey_circumcision$survey_year)

# set start year back a generation if fitting with traditional circumcision
if (inc_time_tmc == TRUE) {
  start_year <- min(c(survey_years, start_year))
  # start_year <- start_year - cens_age
  start_year <- start_year - 50
  # start_year <- start_year - 70
} else {
  # have lower bound on start
  start_year <- min(c(survey_years - 2, start_year)) 
} 


#### Preparing circumcision data ####

# pull latest censoring year from survey_id
cens_year <- max(survey_years)

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circ_preprocess <- prepare_survey_data(
  areas               = areas,
  survey_circumcision = survey_circumcision,
  area_lev            = area_lev,
  start_year          = start_year,
  cens_year           = cens_year,
  cens_age            = cens_age,
  norm_kisk_weights   = TRUE
)

if (nrow(survey_circ_preprocess) == 0) {
  message("no valid surveys at this level") # move inside function!
}

# include indicator to determine whether there is any type distinction for cntry
if (all(is.na(survey_circ_preprocess$circ_who) &
        is.na(survey_circ_preprocess$circ_where))) {
  stop("No type distinction made in valid surveys for this country")
  is_type <- FALSE
  paed_age_cutoff <- NULL
} else is_type <- TRUE


#### Shell dataset to estimate empirical rate ####

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


#### Dataset for modelling ####

dat_tmb <- threemc_prepare_model_data(
  out               = out,
  areas             = areas,
  area_lev          = area_lev,
  aggregated        = TRUE,
  weight            = "population",
  k_dt_age          = k_dt_age,
  paed_age_cutoff   = paed_age_cutoff,
  rw_order          = rw_order, 
  inc_time_tmc      = inc_time_tmc
)

#### Initialise Parameters and define mapped hyperparameters ####

# initial (AR 1) hyperparameters
parameters <- threemc_initial_pars(
  dat_tmb, 
  rw_order        = rw_order, 
  paed_age_cutoff = paed_age_cutoff, 
  inc_time_tmc    = inc_time_tmc
)

# replace parameters with fixed vals
replacement_par_names <- paste0(
  "logsigma_", c("time", "agetime", "spacetime"), "_mmc"
)
replacement_pars <- mget(replacement_par_names)
replacement_pars <- lapply(replacement_pars, as.numeric)
parameters[replacement_par_names] <- replacement_pars

# list of NA factors with names == hyperparameters to fix
maps <- lapply(replacement_pars, function(x) factor(NA))


#### Fit Model with mapped hyperparameters

# function to fit model with fixed hyperpars
# proposal_parameters <- parameters
fit_proposal_model <- function(proposal_parameters, maps) {
  
  # fit proposal TMB model
  set.seed(123)
  fit_fixed <- threemc_fit_model(
    dat_tmb    = dat_tmb,
    parameters = proposal_parameters,
    maps       = maps,
    # randoms    = c(
    #   "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
    #   "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
    #   "u_spacetime_mmc"
    # ),
    randoms    = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc",
      "u_time_tmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
    ),
    N          = N
  )
  
  # subset to specific area level and calculate quantiles for rates and hazard
  out_spec_fixed <- compute_quantiles(out, fit_fixed, area_lev = area_lev) %>%
    # prepare for output
    select(
      area_id, area_name, area_level, year, age, circ_age, population,
      contains("cum_inc"), contains("rate")
    ) %>% 
    # remove years before 2000, to keep save object small
    mutate(n = row_number()) %>% 
    filter(year >= 1998)
  
  # plots to diagnose any issues with fits
  common_plot_aspects <- function(p) {
    p + 
      geom_ribbon(fill = "lightgrey", colour = NA) +
      geom_line(size = 1) +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(
        x = "Age",
        y = "Coverage",
        colour = ""
      ) +
      theme_bw() +
      facet_wrap(. ~ area_name)
  }
  
  # coverage
  cov_plot <- ggplot(
    out_spec_fixed,
    aes(
      x = circ_age,
      y = cum_inc_mmcM,
      ymin = cum_inc_mmcL,
      ymax = cum_inc_mmcU,
      group = as.factor(year),
      colour = as.factor(year)
    )
  ) %>% 
    common_plot_aspects()
  
  rates_plot <- ggplot(
    out_spec_fixed,
    aes(
      x = circ_age,
      y = rate_mmcM,
      ymin = rate_mmcL,
      ymax = rate_mmcU,
      group = as.factor(year),
      colour = as.factor(year)
    )
  ) %>% 
    common_plot_aspects() + 
    labs(y = "Rate")
  
  # perform posterior predictive checks on model
  # ppc_fixed <- threemc_ppc(
  #   fit_fixed,
  #   out_spec_fixed,
  #   survey_circumcision_test,
  #   areas, 
  #   area_lev, 
  #   type = "MMC",
  #   age_groups = five_year_age_groups,
  #   CI_range = CI_range,
  #   N = N
  # )
  ppc_fixed <- threemc_ppc2(
    fit_fixed,
    out_spec_fixed,
    # fix for MWI (??)
    filter(survey_circumcision_test, !is.na(area_id)),
    areas, 
    area_lev, 
    age_groups = five_year_age_groups,
    N = N
  )
  
  # add proposal variance hyperparameters to output
  ppc_fixed$summary_stats$replacement_pars <- replacement_pars
  
  return(list("fit" = fit_fixed,
              "plots" = list(cov_plot, rates_plot),
              "out" = out_spec_fixed,
              "ppc" = ppc_fixed))
}

# ensure failure in fitting proposal model does not lead to failed task
# possibly_fit_proposal_model <- purrr::possibly(fit_proposal_model, NA)

proposal_mod <- fit_proposal_model(parameters, maps)

# minimise fit object for saving
fit_min <- minimise_fit_obj(proposal_mod$fit, dat_tmb, parameters)


#### Save results ####

# Save results
# not required! Never used, using valuable disk space
# data.table::fwrite(
#   proposal_mod$out, file = paste0(save_dir, "Results_DistrictAgeTime_ByType.csv.gz")
# )

# save fit as .rds file
# saveRDS(fit_min, paste0(save_dir, "TMBObjects_DistrictAgeTime_ByType.rds"))

# save ppc df 
readr::write_csv(
  proposal_mod$ppc$ppc_df, file = file.path(save_dir, "pointwise_ppc_df.csv.gz")
)

# save summarised ppc
# saveRDS(proposal_mod$ppc$summary_stats, file.path(save_dir, "ppc_summary.rds"))
readr::write_csv(
  proposal_mod$ppc$ppc_summary_df, file.path(save_dir, "ppc_summary.csv")
)

# save plots 
# ggsave(file.path(save_dir, "Circ_Coverage.pdf"), proposal_mod$plots[[1]])
# ggsave(file.path(save_dir, "Circ_Rates.pdf"), proposal_mod$plots[[2]])
