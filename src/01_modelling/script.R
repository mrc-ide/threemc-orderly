#### Part 1 of Analysis (Modelling) with functionalised behaviour ####

#################
#### Initial ####
#################

### Metadata to run the models

# !! Change this to use dataset stored in threemc
k_dt_age <- 5 # Age knot spacing
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
N <- 1000
forecast_year <- 2021
rw_order <- NULL # AR 1
paed_age_cutoff <- 10
inc_time_tmc <- FALSE

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

# remove circumcisions with missing type?
rm_missing_type <- FALSE

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

# re-calculate start as earliest year - 50 (only where TMC needs to vary)
survey_years <- unique(survey_circumcision$survey_year)
start_year <- min(c(survey_years - 2, 2002)) # have lower bound on start

# Fill in any NAs in populations with
min_pop_year <- min(populations$year)
if (start_year < min_pop_year) {
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
) # %>% 
  # filter(!is.na(population)) # nas in population will cause errors 

#### Dataset for modelling ####

dat_tmb <- threemc_prepare_model_data(
  out               = out,
  areas             = areas,
  area_lev          = area_lev,
  aggregated        = TRUE,
  weight            = "population",
  k_dt_age          = k_dt_age,
  paed_age_cutoff   = paed_age_cutoff
)

#### Modelling circumcision probabilities ####

# Initialise parameter values 
parameters <- threemc_initial_pars(
  dat_tmb, 
  rw_order        = rw_order, 
  paed_age_cutoff = paed_age_cutoff, 
  inc_time_tmc    = inc_time_tmc 
)

# fit model with TMB
fit <- threemc_fit_model(
  dat_tmb    = dat_tmb,
  parameters = parameters,
  randoms    = c(
    "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
    "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
    "u_spacetime_mmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
  ),
  N = N
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
fit <- minimise_fit_obj(fit, dat_tmb, parameters)

# Saving results
data.table::fwrite(
  out_spec, file = paste0(save_dir, "Results_DistrictAgeTime_ByType.csv.gz")
)

# save fit as .rds file
saveRDS(fit, paste0(save_dir, "TMBObjects_DistrictAgeTime_ByType.rds"))

# Plotting results 
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
