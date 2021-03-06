#### Initial ####

## Want to gain intuition on how variance hyperparameters effect error bounds ##
## for model fit ##
# Also fixing correlation hyperparameters as well!

# directory to pull dependencies from
dir_path <- "depends/"
# save locations
save_loc <- "artefacts/"
save_loc_mod <- paste0(save_loc, "models/")
save_loc_agg <- paste0(save_loc, "aggregations/")
# ensure save locs exists
create_dirs_r(save_loc)
create_dirs_r(save_loc_mod)
create_dirs_r(save_loc_agg)


# initial hyperparameters for each modelled country
# init_hyperparameters <- readr::read_csv("data/threemc_hyperpars.csv")
init_hyperparameters <- readr::read_csv(paste0(dir_path, "threemc_hyperpars.csv"))

# Median variance time hyperpars for countries with the widest error bounds
# (for circumcision rate) (which are ZWE, NAM, UGA, ZMB, MOZ):
#logsigma_time_mmc = 0.32550794 (vs -0.05520797 for all countries)
# logsigma_agetime_mmc = 1.82913044 (vs 0.21878839)
# logsigma_spacetime_mmc = -2.30655943 (vs -2.09773799)
# corr: ?
test_hyperparameters <- init_hyperparameters %>%
  filter(iso3 %in% c("ZWE", "NAM", "UGA", "ZMB", "MOZ")) %>%
  # select(contains("logsigma") & contains("time_mmc")) %>%
  select(
    (contains("logsigma") & contains("time_mmc")) | # var hyperpars
      (contains("logitrho_mmc_time"))
  ) %>%
  summarise(across(everything(), median, na.rm = TRUE)) %>%
  # convert to vector
  tidyr::pivot_longer(everything()) %>%
  tibble::deframe()

test_hyperparameters <- round(test_hyperparameters, 9) # ensure hyperparams are known

# remove circumcisions with missing type?
rm_missing_type <- FALSE

#### Metadata to run the models ####
# set country
# cntry <- "MWI"

k_dt <- 5 # Age knot spacing
start_year <-  2006
cens_age <- 59
N <- 1000

#### Reading in data ####
# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# Updated to redefine space here. Previously space variable reset within
# area_level. However, as we will be estimating the rates/coverage across
# multiple admin boundaries (area_level) internally in the model, this needs
# to be reset so that the hazard/integration matrices can distinguish between
# areas/area_levels

# read in data, filter for specific country and male surveys only
filters <- c("iso3" = cntry, sex = "male")
areas_orig <- areas <- read_circ_data(paste0(dir_path, "areas.geojson"), filters)
areas <- areas %>%
  dplyr::mutate(space = 1:dplyr::n()) # add space column to areas
survey_circumcision <- read_circ_data(
  paste0(dir_path, "survey_circumcision.csv.gz"),
  filters
)

if (all(is.na(survey_circumcision$circ_who))) {
  stop("No type distinction for this country, OOS validation non-applicable")
}

populations <- read_circ_data(
  paste0(dir_path, "population_singleage_aggr.csv.gz"),
  filters
)

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

#### Preparing circumcision data ####
# pull latest census year from survey_id
cens_year <- max(as.numeric(
  substr(unique(survey_circumcision$survey_id), 4, 7)
))

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circumcision <- prepare_survey_data(
  areas               = areas,
  # remove area_level column to avoid duplicating columns in prepare_survey_data
  survey_circumcision = select(survey_circumcision, -matches("area_level")),
  area_lev            = area_lev,
  start_year          = start_year,
  cens_year           = cens_year,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
)

if ()

if (nrow(survey_circumcision) == 0) {
  message("no valid surveys at this level") # move inside function!
}

# include indicator to determine whether there is any type distinction for cntry
if (all(is.na(survey_circumcision$circ_who) &
        is.na(survey_circumcision$circ_where))) {
  print("No type distinction made in valid surveys for this country")
  is_type <- FALSE
} else is_type <- TRUE


#### Shell dataset to estimate empirical rate ####

# Skeleton dataset

out <- create_shell_dataset(
  survey_circumcision = survey_circumcision,
  population_data     = populations,
  areas               = areas,
  area_lev            = area_lev,
  time1               = "time1",
  time2               = "time2",
  strat               = "space",
  age                 = "age",
  circ                = "indweight_st"
)

#### Dataset & setup for modelling ####

dat_tmb <- threemc_prepare_model_data(
  out        = out,
  areas      = areas,
  area_lev   = area_lev,
  aggregated = TRUE,
  weight     = "population",
  k_dt       = k_dt
)


# specify TMB model
if (is_type == TRUE) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
} else mod <- "Surv_SpaceAgeTime"

# Initial values
parameters_init <- parameters <- with(
  dat_tmb,
  list(
    # intercept
    "u_fixed_mmc"            = rep(-5, ncol(X_fixed_mmc)),
    "u_fixed_tmc"            = rep(-5, ncol(X_fixed_tmc)),
    # age random effect
    "u_age_mmc"              = rep(0, ncol(X_age_mmc)),
    "u_age_tmc"              = rep(0, ncol(X_age_tmc)),
    # time random effect for MMC
    "u_time_mmc"             = rep(0, ncol(X_time_mmc)),
    # Space random effect (district)
    "u_space_mmc"            = rep(0, ncol(X_space_mmc)),
    "u_space_tmc"            = rep(0, ncol(X_space_tmc)),
    # Interactions for MMC
    "u_agetime_mmc"          = matrix(0, ncol(X_age_mmc), ncol(X_time_mmc)),
    "u_agespace_mmc"         = matrix(0, ncol(X_age_mmc), ncol(X_space_mmc)),
    "u_spacetime_mmc"        = matrix(0, ncol(X_time_mmc), ncol(X_space_mmc)),
    # Interactions for TMC
    "u_agespace_tmc"         = matrix(0, ncol(X_age_tmc), ncol(X_space_tmc)),
    # Autocorrelation parameters for priors
    # Variance
    "logsigma_age_mmc"       = 0,
    "logsigma_time_mmc"      = 0,
    "logsigma_space_mmc"     = 0,
    "logsigma_agetime_mmc"   = 0,
    "logsigma_agespace_mmc"  = 0,
    "logsigma_spacetime_mmc" = 0,
    "logsigma_age_tmc"       = 0,
    "logsigma_space_tmc"     = 0,
    "logsigma_agespace_tmc"  = 0,
    # Mean
    "logitrho_mmc_time1"     = 2,
    "logitrho_mmc_time2"     = 2,
    "logitrho_mmc_time3"     = 2,
    "logitrho_mmc_age1"      = 2,
    "logitrho_mmc_age2"      = 2,
    "logitrho_mmc_age3"      = 2,
    "logitrho_tmc_age1"      = 2,
    "logitrho_tmc_age2"      = 2
  )
)

#### Comparison Fits ####

# set time variance hyperparameters to "test" values
param_order <- names(parameters)
parameters <- parameters[!names(parameters) %in% names(test_hyperparameters)]
parameters <- c(parameters, as.list(test_hyperparameters))
# reorder to correct, original ordering
parameters <- parameters[match(param_order, names(parameters))]

# hold time variance hyperparameters fixed!!
maps <- lapply(rep(NA, length(test_hyperparameters)), factor)
names(maps) <- names(test_hyperparameters)

fit <- threemc_fit_model(
  dat_tmb    = dat_tmb,
  mod        = mod,
  parameters = parameters,
  maps = maps,
  randoms    = c("u_time_mmc", "u_age_mmc", "u_space_mmc",
                 "u_agetime_mmc", "u_agespace_mmc", "u_spacetime_mmc",
                 "u_age_tmc", "u_space_tmc", "u_agespace_tmc"),
  N = N
)

# subset to specific area level and calculate quantiles for rates and hazard
results <- compute_quantiles(out, fit, area_lev = area_lev)

# minimise fit object for saving
fit_save <- minimise_fit_obj(fit, dat_tmb, parameters)

data.table::fwrite(
  results,
  file = paste0(save_loc_mod, "Results_DistrictAgeTime_ByType_test.csv.gz")
)
saveRDS(
  fit_save,
  file = paste0(save_loc_mod, "TMBObjects_DistrictAgeTime_ByType_test.rds")
)

#### Aggregations ####

# load shapefile
areas <- areas_orig %>%
  # Add a unique identifier within Admin code and merging to boundaries
  sf::st_drop_geometry() %>%
  group_by(area_level) %>%
  mutate(space = row_number()) %>%
  ungroup()

# Model with Probability of MC

results$model <- "No program data"
# "small" model fit object
# fit <- readRDS(paste0(
#   mod_loc, "02_", cntry, "_TMBObjects_DistrictAgeTime_ByType_test.rds"
# ))

fit_no_prog <- fit
rm(fit); gc()

## Aggregating ##

# want to aggregate for both discrete ages and "binned" age groups (only need groups)
age_vars <- list("inputs" = c("age_group"), "names" = c("AgeGroup"))
# want to aggregate for various
types <- c("probability", "incidence", "prevalence")

# run aggregations for each combination of age_vars and types
lapply(seq_along(age_vars$inputs), function(i) {
  lapply(seq_along(types), function(j) {
    spec_results <-  threemc_aggregate(
      .data       = results,
      fit         = fit_no_prog,
      areas       = areas,
      populations = populations,
      age_var     = age_vars$inputs[[i]],
      type        = types[j],
      area_lev = area_lev,
      N = N,
      prev_year = 2008 # year to compare with for prevalence
    )
    readr::write_csv(
      x = spec_results,
      file = paste0(
        save_loc_agg, "Results_",
        age_vars$names[[i]], "_", stringr::str_to_title(types[j]),
        "_test.csv.gz"
      )
    )
    rm(spec_results); gc()
  })
})
