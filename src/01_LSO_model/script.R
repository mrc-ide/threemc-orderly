
#### Part 1 of Analysis (Modelling) with functionalised behaviour ####

#################
#### Initial ####
#################

### Metadata to run the models

# !! Change this to use dataset stored in threemc
# pull recommended area hierarchy for target country
area_lev <- threemc::datapack_psnu_area_level %>%
    filter(iso3 == cntry) %>%
    pull(psnu_area_level)
k_dt <- 5 # Age knot spacing
start_year <-  2006
cens_age = 59

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

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

#####################################
#### Preparing circumcision data ####
#####################################

# pull latest census year from survey_id
cens_year <- max(as.numeric(
    substr(unique(survey_circumcision$survey_id), 4, 7)
))

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circumcision <- prepare_survey_data(
    areas = areas,
    survey_circumcision = survey_circumcision,
    survey_individuals = survey_individuals,
    survey_clusters = survey_clusters,
    area_lev = area_lev,
    start_year = start_year,
    cens_year = cens_year,
    cens_age = cens_age,
    norm_kisk_weights = TRUE)

if (nrow(survey_circumcision) == 0) {
    message("no valid surveys at this level")
}

# rm unnecessary datasets
rm(survey_clusters, survey_individuals); gc()


################################################
### Shell dataset to estimate empirical rate ###
################################################

# Skeleton dataset

#' !! JE: the area_id field here used the area_id that appeared in the
#'        circumcision dataset. If there were some area_id with no
#'        observations, then they were dropped, which created a
#'        misalignment of the indexing.  Instead, use the areas dataset
#'        to construct this output frame to ensure all districts are
#'        represented.
#'
#'        I suspect that we also want the circ_age field to be constructed
#'        based on the theoretical maximum circumcision age that we want
#'        outputs for, rather than the maximum observed age; but not 100%
#'        sure.

out <- create_shell_dataset(survey_circumcision,
                            areas,
                            area_lev,
                            time1  = "time1",
                            time2  = "time2",
                            strat  = "space",
                            age    = "age",
                            circ   = "indweight_st")

# remove unnecessary datasets
rm(survey_circumcision); gc()

###############################
#### Dataset for modelling ####
##############################

#' create design matrices for fixed effects and temporal, age, space and
#' interaction random effects
design_matrices <- create_design_matrices(out = out, k_dt = k_dt)

# create survival matrices for MMC, TMC, censored and left censored
survival_matrices <- create_survival_matrices(out,
                                              time1 = "time1",
                                              time2 = "time2",
                                              age = "age",
                                              strat = "space")

# create integration matrices for selecting the instantaneous hazard rate
integration_matrices <- create_integration_matrices(out,
                                                    time1 = "time1",
                                                    time2 = "time2",
                                                    age = "age",
                                                    strat = "space")

# Precision/Adjacency matrix for the spatial random effects
Q_space <- list("Q_space" = create_icar_prec_matrix(
  sf_obj    = filter(areas, area_level == area_lev),
  row.names = "space"
  )
)

# Combine Data for tmb model
# (could do this by looping through `create_matrices` functions!)
dat_tmb <- c(design_matrices, survival_matrices, integration_matrices, Q_space)

rm(design_matrices, survival_matrices, integration_matrices, Q_space); gc()

##########################################################
### Modelling circumcision rate over time (AR Process) ###
##########################################################


# compile TMB model
compile_tmb("Surv_SpaceAgeTime_ByType.cpp")
dyn.load(TMB::dynlib("Surv_SpaceAgeTime_ByType"))

# Initial values
parameters <- with(
  dat_tmb,
  list(
    # intercept
    u_fixed_mmc            = rep(-5, ncol(X_fixed_mmc)),
    u_fixed_tmc            = rep(-5, ncol(X_fixed_tmc)),
    # age random effect
    u_age_mmc              = rep(0, ncol(X_age_mmc)),
    u_age_tmc              = rep(0, ncol(X_age_tmc)),
    # time random effect for MMC
    u_time_mmc             = rep(0, ncol(X_time_mmc)),
    # Space random effect (district)
    u_space_mmc            = rep(0, ncol(X_space_mmc)),
    u_space_tmc            = rep(0, ncol(X_space_tmc)),
    # Interactions for MMC
    u_agetime_mmc          = matrix(0, ncol(X_age_mmc), ncol(X_time_mmc)),
    u_agespace_mmc         = matrix(0, ncol(X_age_mmc), ncol(X_space_mmc)),
    u_spacetime_mmc        = matrix(0, ncol(X_time_mmc), ncol(X_space_mmc)),
    # Interactions for TMC
    u_agespace_tmc         = matrix(0, ncol(X_age_tmc), ncol(X_space_tmc)),
    # Autocorrelation parameters for priors
    # Variance
    logsigma_age_mmc       = 0,
    logsigma_time_mmc      = 0,
    logsigma_space_mmc     = 0,
    logsigma_agetime_mmc   = 0,
    logsigma_agespace_mmc  = 0,
    logsigma_spacetime_mmc = 0,
    logsigma_age_tmc       = 0,
    logsigma_space_tmc     = 0,
    logsigma_agespace_tmc  = 0,
    # Mean
    logitrho_mmc_time1     = 2,
    logitrho_mmc_time2     = 2,
    logitrho_mmc_time3     = 2,
    logitrho_mmc_age1      = 2,
    logitrho_mmc_age2      = 2,
    logitrho_mmc_age3      = 2,
    logitrho_tmc_age1      = 2,
    logitrho_tmc_age2      = 2
  )
)

# Creating TMB object
obj <- TMB::MakeADFun(dat_tmb,
                 parameters,
                 random = c("u_time_mmc", "u_age_mmc", "u_space_mmc",
                            "u_agetime_mmc", "u_agespace_mmc",
                            "u_spacetime_mmc", "u_age_tmc",
                            "u_space_tmc", "u_agespace_tmc"),
                 method = "BFGS",
                 hessian = TRUE,
                 DLL = "Surv_SpaceAgeTime_ByType")

rm(dat_tmb, parameters); gc()

# Running optimiser
opt <- stats::nlminb(start   = obj$par,
              obj     = obj$fn,
              gr      = obj$gr,
              control = list(trace = 1))

# sample from TMB fit
fit <- circ_sample_tmb(obj, opt, nsample = 1000)

rm(obj, opt); gc()

# calculate quantiles for rates and cumulative hazard
out <- compute_quantiles(out, fit)

######################
### saving results ###
######################
# preparing for output (could surely put this in function and make more terse!)
out <- out %>%
    dplyr::select(c(area_id, area_name, year, age = circ_age, #population,
                    obs_mmc, obs_tmc, cens, icens, N,
                    rate_mmcM, rate_mmcL, rate_mmcU,
                    rate_tmcM, rate_tmcL, rate_tmcU,
                    rateM, rateL, rateU,
                    survM, survL, survU,
                    inc_tmcM, inc_tmcL, inc_tmcU,
                    inc_mmcM, inc_mmcL, inc_mmcU,
                    incM, incL, incU,
                    cum_inc_tmcM, cum_inc_tmcL, cum_inc_tmcU,
                    cum_inc_mmcM, cum_inc_mmcL, cum_inc_mmcU,
                    cum_incM, cum_incL, cum_incU))


# Saving results (also make into function)
data.table::fwrite(out, file = "Results_DistrictAgeTime_ByType.csv.gz")
save(fit, file = "TMBObjects_DistrictAgeTime_ByType.RData")

# Plotting results (make this into a diagnostics plot kind of function)
# Coverage
# pdf(here::here(paste0("Runs/", cntry, "_Coverage.pdf")), width = 10)
# ggplot(out,
#        aes(x = age,
#            y = cum_incM,
#            ymin = cum_incL,
#            ymax = cum_incU,
#            group = as.factor(year),
#            colour = as.factor(year))) +
#   geom_ribbon(fill = "lightgrey",
#               colour = NA) +
#   geom_line(size = 1) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   labs(x = "Age",
#        y = "Coverage",
#        colour = "") +
#   theme_bw() +
#   facet_wrap(. ~ area_name)
# dev.off()
# 
# # Rates
# pdf(here::here(paste0("Runs/", cntry, "_Rates.pdf")), width = 10)
# ggplot(out,
#        aes(x = age,
#            y = rateM,
#            ymin = rateL,
#            ymax = rateU,
#            group = as.factor(year),
#            colour = as.factor(year))) +
#   geom_ribbon(fill = "lightgrey",
#               colour = NA) +
#   geom_line(size = 1) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   labs(x = "Age",
#        y = "Rates",
#        colour = "") +
#   theme_bw() +
#   facet_wrap(. ~ area_name)
# dev.off()


# Clearing Workspace
# rm(list = ls()); gc()