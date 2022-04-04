
#### Part 1 of Analysis (Modelling) with functionalised behaviour ####

#### Initial ####

## Metadata to run the models

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
  "population_singleage_aggr.csv.gz",
  filters
)


# pull recommended area hierarchy for target country
area_lev <- threemc::datapack_psnu_area_level %>%
  filter(iso3 == cntry) %>%
  pull(psnu_area_level)
if (area_lev == 0) area_lev <- NULL # don't model at the country level

# if area_level is missing, assume most common area lev in surveys
if (length(area_lev) == 0) {
  area_lev <- table(as.numeric(substr(survey_clusters$geoloc_area_id, 5, 5)))
  area_lev <- as.numeric(names(area_lev)[area_lev == max(area_lev)])
}

#### Data Prep ####

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
  rm_missing_type = rm_missing_type,
  norm_kisk_weights = TRUE)

if (nrow(survey_circumcision) == 0) {
  message("no valid surveys at this level")
}

# include indicator to determine whether there is any type distinction for cntry
if (all(is.na(survey_circumcision$circ_who) &
        is.na(survey_circumcision$circ_where))) {
  print("No type distinction made in valid surveys for this country")
  is_type <- FALSE
} else is_type <- TRUE


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

#### First Model Setup ####

# specify model
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

## fixed effects ##

# set all time variance parameters to specified values (default is median for all countries)
parameters["logsigma_time_mmc"] <- logsigma_time_mmc_val
parameters["logsigma_agetime_mmc"] <- logsigma_agetime_mmc_val
parameters["logsigma_spacetime_mmc"] <- logsigma_spacetime_mmc_val

# hold these parameters fixed
# fixed_effects <- lapply(rep(NA, length(initial_params)), factor)
# names(fixed_effects) <- names(initial_params)
fixed_effects <- list(
  logsigma_time_mmc = factor(NA),
  logsigma_agetime_mmc = factor(NA),
  logsigma_spacetime_mmc = factor(NA)
)

# Create TMB object
obj <- TMB::MakeADFun(dat_tmb,
                      parameters,
                      map = fixed_effects,
                      random = randoms,
                      method = "BFGS",
                      hessian = TRUE,
                      DLL = mod)

# Run optimiser
# opt <- stats::nlminb(
#   start   = obj$par,
#   obj     = obj$fn,
#   gr      = obj$gr,
#   control = list(trace = 1)
# )
opt <- do.call(optim, obj)

# sample from TMB fit
fit <- circ_sample_tmb(obj, opt, nsample = N)

# calculate quantiles for rates and cumulative hazard
out <- compute_quantiles(out, fit)

#### saving results ####

# prepare for output (could surely put this in function and make more terse!)
out <- out %>% # no obs_mc?
  select(
    area_id, area_name, year, age = circ_age,
    contains("obs"),
    cens, icens, N,
    contains("rate_mmc"), contains("rate_tmc"), contains("rate"),
    contains("surv"),
    contains("cum_inc_mmc"), contains("cum_inc_tmc"), contains("cum_inc"),
    contains("inc_mmc"), contains("inc_tmc"), contains("inc")
  )

# Saving results (also make into function)
data.table::fwrite(out, file = "Results_DistrictAgeTime_ByType.csv.gz")

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
ggplot(out,
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
ggplot(out,
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

#### Aggregation #### 

aggregation_fun <- function(fit, results, areas, area_lev, populations,
                            spec_type = c("probability", "incidence", "prevalence")) {
    # Number of samples to use
    N <- 100

    ### Preparing location/shapefile information ###
    # Load shapefile
    if (inherits(areas, "sf")) {
        areas <- sf::st_drop_geometry(areas)
    }
    areas <- areas %>%
        # Add a unique identifier within Admin code and merging to boundaries
        group_by(area_level) %>%
        mutate(space = row_number()) %>%
        ungroup()

    # wide formatted areas, for changing area levels later
    areas_wide <- areas %>%
        select(area_id, area_name, parent_area_id, area_level) %>%
        naomi::spread_areas()

    # Model with Probability of MC
    results$model <- "No program data"
    fit_no_prog <- fit # need to load model with programme data as well

    ### Loading rates from survival model ###
    results <- prepare_sample_data(N = N,
                                   populations = populations,
                                   no_prog_results = results,
                                   no_prog_tmb_fit = fit_no_prog,
                                   type = spec_type)

    rm(fit, fit_no_prog, populations); gc()

    ####################################
    ### Preparing results for output ###
    ####################################

    # collect results for lower area hierarchies by joining higher area
    # hierarchies (do so until you reach "area 0")
    results_list <- combine_areas(results, areas_wide, area_lev, join = FALSE)

    # aggregate samples for each individual age group
    results <- aggregate_sample_age_group(results_list)

    if (spec_type == "prevalence") {
        # calculate change in prevalence since 2008
        results_change_2008 <- prevalence_change(results, spec_year = 2008)

        ### Getting number of people circumcised ###

        results_n <- n_circumcised(results)

        ### Preparing results for output ###

        # add "change from 2008" and "n_circumcised" results to "coverage" results
        results <- rbind(results, results_change_2008, results_n)
    }
    results <- posterior_summary_fun(results)

    # Merging regional information on the dataset (i.e. parent area info)
    results <- merge_area_info(results, areas)
}

types <- c("probability", "incidence", "prevalence")

lapply(types, function(x) {
  aggregation <- aggregation_fun(
    fit,
    out,
    areas,
    area_lev,
    populations,
    spec_type = x
  )
  data.table::fwrite(
    aggregation,
    file = here::here(
      paste0(
        "Results_AgeGroup_",
        stringr::str_to_title(x), ".csv.gz"
      )
    )
  )
})
