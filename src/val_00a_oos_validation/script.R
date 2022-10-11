#### Perform Modelling and Aggregation With Latest Survey Removed ####  

#### Initial ####

# directory to pull dependencies from
dir_path <- "depends/"
# save location
save_loc <- "artefacts/"
# ensure save loc exists
create_dirs_r(save_loc)

# remove circumcisions with missing type?
rm_missing_type <- FALSE


#### Metadata to run the models ####

# set country
# cntry <- "MWI"

k_dt <- 5 # Age knot spacing
start_year <-  2002
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
forecast_year <- 2021
paed_age_cutoff <- 10


#### Reading in data ####

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

if (is.null(area_lev)) {
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
}
  

#### remove most recent survey #### 

# If second last survey year is just one year previous, also remove it

# add survey year column
survey_circumcision <- survey_circumcision %>% 
  mutate(survey_year = as.numeric(substr(survey_circumcision$survey_id, 4, 7)))

# find survey years, rm max year (and also second largest year, if appropriate)
survey_years <- sort(unique(survey_circumcision$survey_year))
max_years <- max(survey_years)
if (length(survey_years) > 1) {
  if ((max_years - survey_years[length(survey_years) - 1]) == 1) {
    max_years <- c(max_years, max_years - 1)
  }
  # record removed surveys
  removed_surveys <- survey_circumcision %>% 
    filter(survey_year %in% max_years) %>% 
    distinct(survey_id) %>% 
    pull()
  survey_circumcision <- survey_circumcision %>% 
    filter(!survey_year %in% max_years)
} else {
  # don't continue if there is only one survey available for a country
  stop(paste0(
    "Only one survey available for ", 
    cntry, 
    ", so OOS Validation not possible")
  )
}

# save used and unused surveys
used_surveys <- paste(unique(survey_circumcision$survey_id), collapse = ", ")
survey_info <- data.frame(
  "used_surveys" = used_surveys,
  "removed_surveys" = removed_surveys
)
readr::write_csv(survey_info, paste0(save_loc, "used_survey_info.csv"))


#### Preparing circumcision data ####

# pull latest and first censoring year from survey_id
survey_years <- as.numeric(substr(unique(survey_circumcision$survey_id), 4, 7))

cens_year <- max(survey_years)
start_year <- min(c(survey_years - 2, start_year)) # have lower bound on start

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circ_preprocess <- prepare_survey_data(
  areas               = areas,
  # remove area_level column to avoid duplicating columns in prepare_survey_data
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
  print("No type distinction made in valid surveys for this country")
  is_type <- FALSE 
  paed_age_cutoff <- NULL
} else is_type <- TRUE


#### Shell dataset to estimate empirical rate ####

 


# create shell dataset from surveys
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
    out             = out,
    areas           = areas,
    area_lev        = area_lev,
    aggregated      = TRUE,
    weight          = "population",
    k_dt            = k_dt,
    paed_age_cutoff = paed_age_cutoff
)


#### Modelling circumcision probabilites ####

# specify TMB model, depending on whether type distinction is available
if (is_type == TRUE) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC"
} else {
  mod <- "Surv_SpaceAgeTime"
  # empty df for paed design matrices so parameter assignment doesn't fail
  X_fixed_mmc_paed <- X_age_mmc_paed <- X_space_mmc_paed <- data.frame(0)
}

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
    "logitrho_tmc_age1"           = 2,
    "logitrho_tmc_age2"           = 2
  )
)

if (is_type == FALSE) {
  # remove paed-related parameters
  parameters <- parameters[!grepl("paed", names(parameters))]
}

fit <- threemc_fit_model(
    dat_tmb    = dat_tmb,
    mod        = mod,
    parameters = parameters,
    randoms    = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
    ), 
    N          = 1000
)

# subset to specific area level and calculate quantiles for rates and hazard
out_spec <- compute_quantiles(out, fit, area_lev = area_lev)


#### saving results ####

# prepare for output
out_spec <- out_spec %>%
  select(
    area_id, area_name, area_level, year, age = circ_age,
    contains("obs"),
    cens, icens, N,
    contains("rate_mmc"), contains("rate_tmc"), contains("rate"),
    contains("surv"),
    contains("cum_inc_mmc"), contains("cum_inc_tmc"), contains("cum_inc"),
    contains("inc_mmc"), contains("inc_tmc"), contains("inc")
  )

# minimise fit object for saving
fit_min <- minimise_fit_obj(fit, dat_tmb, parameters)

# Saving results (also make into function)
readr::write_csv(
  out_spec,
  file = paste0(save_loc, "Results_DistrictAgeTime_ByType_OOS.csv.gz")
)

# save minimised fit obj as .rds file
saveRDS(
  fit_min, 
  paste0(save_loc, "TMBObjects_DistrictAgeTime_ByType_OOS.rds")
)
rm(fit_min); gc()

# Plotting results (make this into a diagnostics plot kind of function)
# Coverage
# pdf(paste0(save_loc, "_Coverage.pdf"), width = 10)
cov_plt <- ggplot(out_spec,
       aes(x = age,
           y = cum_incM,
           ymin = cum_incL,
           ymax = cum_incU,
           group = as.factor(year),
           colour = as.factor(year))) +
  geom_ribbon(fill = "lightgrey",
              colour = NA) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Age",
       y = "Coverage",
       colour = "") +
  theme_bw() +
  facet_wrap(. ~ area_name)
# dev.off()
ggsave(plot = cov_plt, filename = paste0(save_loc, "Circ_Coverage.pdf"), width = 10)

# Rates
# pdf(paste0(save_loc, "_Rates.pdf"), width = 10)
rate_plt <- ggplot(out_spec,
       aes(x = age,
           y = rateM,
           ymin = rateL,
           ymax = rateU,
           group = as.factor(year),
           colour = as.factor(year))) +
  geom_ribbon(fill = "lightgrey",
              colour = NA) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Age",
       y = "Rates",
       colour = "") +
  theme_bw() +
  facet_wrap(. ~ area_name)
# dev.off()
ggsave(plot = rate_plt, filename = paste0(save_loc, "Circ_Rates.pdf"), width = 10)

#### Aggregation ####

# re-sample from model, if required (shouldn't be!)
if (is.null(fit$sample)) {
  fit <- threemc_fit_model(
    fit     = fit,
    mod     = mod,
    randoms = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
    ), 
    N       = 1000
  )
}

fit_no_prog <- fit
rm(fit); gc()

# want to aggregate for both discrete ages and "binned" age groups
# age_vars <- list("inputs" = c("age", "age_group"), "names" = c("Age", "AgeGroup"))
age_vars <- list("inputs" = c("age_group"), "names" = c("AgeGroup"))
# want to aggregate for various types
types <- c("probability", "incidence", "prevalence")

# run aggregations for each combination of age_vars and types
# aggregations <- lapply(seq_along(age_vars$inputs), function(i) {
lapply(seq_along(age_vars$inputs), function(i) {
  lapply(seq_along(types), function(j) {
    spec_results <-  threemc_aggregate(
      .data       = out_spec,
      fit         = fit_no_prog,
      areas       = areas,
      populations = populations,
      age_var     = age_vars$inputs[[i]],
      type        = types[j],
      area_lev    = area_lev,
      N           = N,
      prev_year   = 2008 # year to compare with for prevalence
    )
    readr::write_csv(
        x = spec_results,
        file = paste0(
            save_loc, "Results_",
            age_vars$names[[i]], "_", stringr::str_to_title(types[j]),
            "_OOS.csv.gz"
        )
    )

    rm(spec_results); gc()
  })
})
