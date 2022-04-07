
#### Part 1 of Analysis (Modelling) with functionalised behaviour ####

#################
#### Initial ####
#################

### Metadata to run the models

# !! Change this to use dataset stored in threemc
k_dt <- 5 # Age knot spacing
start_year <-  2006
cens_age <- 59
N <- 1000

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# remove circumcisions with missing type?
rm_missing_type <- FALSE

# read in data, filter for specific country and male surveys only
filters <- c("iso3" = cntry, sex = "male")
areas <- read_circ_data(
    "areas.geojson",
    filters
)
areas <- st_make_valid(areas) # move this to function
survey_clusters <- read_circ_data("survey_clusters.csv.gz", filters)
survey_individuals <- read_circ_data("survey_individuals.csv.gz", filters)
survey_circumcision <- read_circ_data(
  "survey_circumcision.csv.gz", 
  filters
)
# also read in population data
populations <- read_circ_data(
  here::here("population_singleage.csv.gz"),
  filters
)


# pull recommended area hierarchy for target country
area_lev <- threemc::datapack_psnu_area_level %>%
  filter(iso3 == cntry) %>%
  pull(psnu_area_level)
# don't model at the country level
if (length(area_lev) != 0 && area_lev == 0) area_lev <- NULL 

# if area_level is missing, assume most common area lev in surveys
if (length(area_lev) == 0) {
  area_lev <- table(as.numeric(substr(survey_clusters$geoloc_area_id, 5, 5)))
  area_lev <- as.numeric(names(area_lev)[area_lev == max(area_lev)])
}

#####################################
#### Preparing circumcision data ####
#####################################
# pull latest census year from survey_id
cens_year <- max(as.numeric(
  substr(unique(survey_circumcision$survey_id), 4, 7)
))

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circumcision <- prepare_survey_data(
  areas               = areas,
  survey_circumcision = survey_circumcision,
  survey_individuals  = survey_individuals,
  survey_clusters     = survey_clusters,
  area_lev            = area_lev,
  start_year          = start_year,
  cens_year           = cens_year,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
)

if (nrow(survey_circumcision) == 0) {
  message("no valid surveys at this level")
}

# include indicator to determine whether there is any type distinction for cntry
if (all(is.na(survey_circumcision$circ_who) &
        is.na(survey_circumcision$circ_where))) {
  print("No type distinction made in valid surveys for this country")
  is_type <- FALSE
} else is_type <- TRUE


# rm unnecessary datasets
rm(survey_clusters, survey_individuals); gc()

################################################
### Shell dataset to estimate empirical rate ###
################################################

# Skeleton dataset

# Shell dataset creation changed in the following ways:
#    1) Single age population counts are now added to the output data set.
#       This is needed early on, as we will be aggregating the estimated
#       probabilities and cumulative incidence from the model on the district
#       level in order to include survey data not on the district level (or
#       administrative level of interest). These will be weighted by population.
#    2) Now produced for multiple levels of area_level. Function sets
#       up the shell dataset for all admin boundaries between national (admin 0)
#       and the district level (or administrative level of interest) rather
#       than letting survey_circumcision dictate one level of interest
#
# The internal aggregating to get the obs_mmc etc. still works as the functions
# now uses the new "space" variable defined above. These functions treat each
# "space" as a stratification variable and therefore self-contained. This has
# implications later where we have to specify the administrative boundaries
# we are primarily modelling on.

# TODO: Some countries only have the single age population on the aggregation
# of interest so will need to be changed as we roll this update out.
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

# remove unnecessary datasets
rm(survey_circumcision); gc()

#############################
### Dataset for modelling ###
#############################

# Create design matrices for fixed effects and temporal, age, space and
# interaction random effects
#
# Code has been updated to only produce the design matrices on the lowest
# area_level as the shell dataset contains multiple area_levels now.
design_matrices <- create_design_matrices(dat = out,
                                          area_lev = area_lev,
                                          k_dt = k_dt)

# Create integration matrices for selecting the instantaneous hazard rate
#
# Code has been updated to produce the integration matrices on the lowest
# area_level as the shell dataset contains multiple area_levels now.
#
# Updated both integration matrices functions so that the two cases where
# we have and don't have a stratification variable are dealt with the same
# piece of code. Done by introducing a dummy variable when no stratification
# variables specified.
integration_matrices <- create_integration_matrices(out,
                                                    area_lev = area_lev,
                                                    time1 = "time1",
                                                    time2 = "time2",
                                                    age = "age",
                                                    strat = "space")

# The function was first updated so that the two cases where we have and don't
# have a stratification variable are dealt with the same piece of code. Done
# by introducing a dummy variable when no stratification variables specified.
#
# The secondly the function now considers two cases when we want to run (a)
# on all areas and (b) aggregated from the reference area level. The former
# is needed to calculate the PYRS and the latter is needed for the likelihood
# of the function.
# create survival matrices for MMC, TMC, censored and left censored
survival_matrices <- create_survival_matrices(out,
                                              areas = areas,
                                              area_lev = area_lev,
                                              time1 = "time1",
                                              time2 = "time2",
                                              age = "age",
                                              strat = "space",
                                              aggregated = TRUE,
                                              weight = 'population')

# Precision/Adjacency matrix for the spatial random effects
Q_space <- list("Q_space" =
                  create_icar_prec_matrix(sf_obj    = areas,
                                          area_lev = area_lev,
                                          row.names = "space"))

# Combine Data for tmb model
# (could do this by looping through `create_matrices` functions!)
dat_tmb <- c(design_matrices, survival_matrices, integration_matrices, Q_space)

rm(design_matrices, survival_matrices, integration_matrices, Q_space); gc()

###########################################
### Modelling circumcision probabilites ###
###########################################
# specify TMB model
if (is_type == TRUE) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
} else mod <- "Surv_SpaceAgeTime"

# Initial values
parameters <- with(
  dat_tmb,
  list(
    # intercept
    "u_fixed_mmc"          = rep(-5, ncol(X_fixed_mmc)),
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
    "logitrho_tmc_age1"     = 2,
    "logitrho_tmc_age2"      = 2
  )
)

# random effects
randoms <- c("u_time_mmc", "u_age_mmc", "u_space_mmc",
             "u_agetime_mmc", "u_agespace_mmc", "u_spacetime_mmc",
             "u_age_tmc", "u_space_tmc", "u_agespace_tmc")

if (is_type == FALSE) {
  
  remove_type_distinction <- function(x) {
    names(x) <- stringr::str_remove(names(x), "_mmc")
    x <- x[!names(x) %like% "_tmc"]
  }
  
  dat_tmb <- remove_type_distinction(
    dat_tmb[!names(dat_tmb) %in% c("A_mmc", "A_tmc")]
  )
  names(dat_tmb)[names(dat_tmb) == "A_mc"] <- "A"
  
  parameters <- remove_type_distinction(parameters)
  
  randoms <- stringr::str_remove(randoms, "_mmc")
  randoms <- randoms[!randoms %like% "_tmc"]
  # randoms <- randoms[!randoms %in% c("u_space"     "u_agespace"  "u_spacetime")]
}

# ?
randoms <- randoms[randoms %in% names(parameters)]
if (length(randoms) == 0) {
  randoms <- NULL
}

# Create TMB object
obj <- TMB::MakeADFun(dat_tmb,
                      parameters,
                      random = randoms,
                      method = "BFGS",
                      hessian = TRUE,
                      DLL = mod)

rm(dat_tmb, parameters); gc()

# Run optimiser
opt <- stats::nlminb(
  start   = obj$par,
  obj     = obj$fn,
  gr      = obj$gr,
  control = list(trace = 1)
)

# sample from TMB fit
fit <- circ_sample_tmb(obj, opt, nsample = N)

rm(obj, opt); gc()

# subset to specific are level
out_spec <- out %>%
  filter(area_level == area_lev)

# calculate quantiles for rates and cumulative hazard
out_spec <- compute_quantiles(out_spec, fit)

######################
### saving results ###
######################
# prepare for output (could surely put this in function and make more terse!)
out_spec <- out_spec %>% # no obs_mc?
  select(
    area_id, area_name, area_level, year, age = circ_age,
    contains("obs"),
    cens, icens, N,
    contains("rate_mmc"), contains("rate_tmc"), contains("rate"),
    contains("surv"),
    contains("cum_inc_mmc"), contains("cum_inc_tmc"), contains("cum_inc"),
    contains("inc_mmc"), contains("inc_tmc"), contains("inc")
  )

# Saving results (also make into function)
data.table::fwrite(out, file = "Results_DistrictAgeTime_ByType_full.csv.gz")
data.table::fwrite(out_spec, file = "Results_DistrictAgeTime_ByType.csv.gz")

# save smaller TMB object
fit_small <- fit
fit_small$tmb_data <- dat_tmb
fit_small$par_init <- parameters
fit_small$sample <- NULL
fit_small$obj <- NULL
saveRDS(fit_small, "TMBObjects_DistrictAgeTime_ByType.rds")

# Plotting results 
# Coverage
pdf("Circ_Coverage.pdf", width = 10)
ggplot(out_spec,
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
dev.off()

# Rates
pdf("Circ_Rates.pdf", width = 10)
ggplot(out_spec,
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
dev.off()
