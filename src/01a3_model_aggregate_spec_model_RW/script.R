#### Model with user-specified model ####

#################
#### Initial ####
#################

# stopifnot(mod %in% c(
#   "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC"
#   # "Surv_SpaceAgeTime_ByType_withUnknownType_RW",
#   # "Surv_SpaceAgeTime_ByType_withUnknownType_RW2",
#   # "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC_RW2"
# ))

stopifnot(rw_order %in% c(1, 2))

# mod <- "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC_RW"

### Metadata to run the models

# !! Change this to use dataset stored in threemc
k_dt <- 5 # Age knot spacing
start_year <-  2002
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
forecast_year <- 2021
paed_age_cutoff <- NULL
# if (mod == "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC_RW") {
  paed_age_cutoff <- 10
# }
# rw_order <- NULL # use RW or AR temporal prior?
inc_time_tmc <- FALSE # don't include time random effect for TMC
# if (grepl("2", mod)) inc_time_tmc <- TRUE

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

# remove circumcisions with missing type?
rm_missing_type <- FALSE

# read in data, filter for specific country and male surveys only
filters <- c("iso3" = cntry, sex = "male")
areas_orig <- areas <- read_circ_data("depends/areas.geojson", filters) 

areas <- areas %>% 
  dplyr::mutate(space = 1:dplyr::n()) # add space column to areas
areas <- st_make_valid(areas) 
survey_circumcision <- read_circ_data("depends/survey_circumcision.csv.gz", filters)
populations <- read_circ_data("depends/population_singleage_aggr.csv.gz", filters)

# calculate start as earliest survey year - 50 (only where TMC needs to vary)
survey_years <- as.numeric(substr(unique(survey_circumcision$survey_id), 4, 7))
if (inc_time_tmc == TRUE) {
  start_year <- first(survey_years) - 50
} else start_year <- 2002

# pull recommended area hierarchy for target country
if (!is.null(area_lev) && !is.numeric(area_lev)) {
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


#### Preparing circumcision data ####

# pull latest and first censoring year from survey_id
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
is_type <- TRUE
if (all(is.na(survey_circ_preprocess$circ_who) &
        is.na(survey_circ_preprocess$circ_where))) {
  # stop("No type distinction made in valid surveys for this country")
  is_type = FALSE
  # can't have paediatric MMC age cutoff with no type info!
  paed_age_cutoff <- NULL 
} 


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

# Temp: fill in NAs in population with next known value
# (not very correct but will do for now!!)
# if (!all(!is.na(out$population))) {
#   message("Filling in missing populations with earliest known value")
#   out <- out %>%
#     group_by(area_id, area_name, area_level, space, circ_age, age) %>%
#     tidyr::fill(population, .direction = "downup") %>%
#     ungroup()
# }

#### Dataset for modelling ####

dat_tmb <- threemc_prepare_model_data(
  out               = out,
  areas             = areas,
  area_lev          = area_lev,
  aggregated        = TRUE,
  weight            = "population",
  k_dt              = k_dt,
  rw_order          = rw_order,
  paed_age_cutoff   = paed_age_cutoff,
  inc_time_tmc      = inc_time_tmc
)

#### Modelling circumcision probabilites ####

# specify TMB model, depending on whether type distinction is available
# if (is_type == TRUE) {
#   mod <- "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC"
# } else {
#   mod <- "Surv_SpaceAgeTime"
#   # empty df for paed design matrices so parameter assignment doesn't fail
#   X_fixed_mmc_paed <- X_age_mmc_paed <- X_space_mmc_paed <- data.frame(0)
# }

# initialise parameters
parameters <- threemc_initial_pars(
  dat_tmb         = dat_tmb, 
  rw_order        = rw_order, 
  paed_age_cutoff = paed_age_cutoff, 
  inc_time_tmc    = inc_time_tmc
)

# fit model with TMB
fit <- threemc_fit_model(
  dat_tmb    = dat_tmb,
  # mod        = mod,
  parameters = parameters,
  randoms    = c(
    "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
    "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
    "u_spacetime_mmc",
    "u_time_tmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
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
fit_save <- minimise_fit_obj(fit, dat_tmb, parameters)

# plot coverage and rates to check fit
# Coverage
pdf(paste0(save_dir, "Circ_Coverage.pdf"), width = 10)
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
pdf(paste0(save_dir, "Circ_Rates.pdf"), width = 10)
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


# Saving results
data.table::fwrite(
  out_spec, file = paste0(save_dir, "Results_DistrictAgeTime_ByType.csv.gz")
)

# save fit as .rds file
saveRDS(fit_save, paste0(save_dir, "TMBObjects_DistrictAgeTime_ByType.rds"))
rm(fit_save); gc()

#### Aggregations ####

areas <- areas_orig %>% 
  # Add a unique identifier within Admin code and merging to boundaries
  sf::st_drop_geometry() %>%
  group_by(area_level) %>%
  mutate(space = row_number()) %>%
  ungroup()

# re-sample from model, if required (shouldn't be!)
if (is.null(fit$sample)) {
  fit <- threemc_fit_model(
    fit     = fit,
    mod     = mod,
    randoms = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc",
      "u_time_tmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
    ),
    N       = 1000
  )
}

fit_no_prog <- fit
rm(fit); gc()

# Fill in any NAs in populations for modelled years with last known value
min_pop_year <- min(populations$year)
if (any(out_spec$year < min_pop_year)) {
  missing_years <- start_year:(min_pop_year - 1)
  missing_rows <- tidyr::crossing(
    select(populations, -c(year, population)),
    "year"       = missing_years,
    "population" = NA
  )
  populations <- bind_rows(populations, missing_rows) %>%
    arrange(iso3, area_id, area_level, age, year) %>%
    group_by(iso3, area_id, area_level, age) %>%
    tidyr::fill(population, .direction = "downup") %>%
    ungroup()
}

# want to aggregate for both discrete ages and "binned" age groups
# age_vars <- list("inputs" = c("age", "age_group"), "names" = c("Age", "AgeGroup"))
age_vars <- list("inputs" = c("age_group"), "names" = c("AgeGroup"))
# want to aggregate for various types
# types <- c("prevalence")
types <- c("probability", "incidence", "prevalence")

# run aggregations for each combination of age_vars and types
# aggregations <- lapply(seq_along(age_vars$inputs), function(i) {
lapply(seq_along(age_vars$inputs), function(i) {
  lapply(seq_along(types), function(j) {
    spec_results <- threemc_aggregate(
      .data       = out_spec,
      fit         = fit_no_prog,
      areas       = areas,
      populations = populations,
      age_var     = age_vars$inputs[[i]],
      type        = types[j],
      area_lev    = area_lev,
      N           = N,
      prev_year   = NULL # year to compare with for prevalence
    )
    readr::write_csv(
      x = spec_results,
      file = paste0(
        save_dir, "Results_",
        age_vars$names[[i]], "_", stringr::str_to_title(types[j]),
        ".csv.gz"
      )
    )
    rm(spec_results); gc()
  })
})
