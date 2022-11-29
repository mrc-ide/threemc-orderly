#### Part 1 of Analysis (Modelling) with functionalised behaviour ####

#################
#### Initial ####
#################

### Metadata to run the models

# !! Change this to use dataset stored in threemc
k_dt <- 5 # Age knot spacing
start_year <-  2002
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
N <- 1000
forecast_year <- 2021
# remove paediatric age cutoff and rw_order, if non-numeric
if (!is.numeric(paed_age_cutoff)) paed_age_cutoff <- NULL
if (!is.numeric(rw_order)) rw_order <- NULL
# inc_time_tmc <- TRUE

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
survey_circumcision <- read_circ_data("depends/survey_circumcision.csv.gz", filters)
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

#### Preparing circumcision data ####
# pull latest and first censoring year from survey_id
survey_years <- as.numeric(substr(unique(survey_circumcision$survey_id), 4, 7))

cens_year <- max(survey_years)
start_year <- max(min(survey_years), start_year) # have lower bound on start

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

# take start year for skeleton dataset from surveys 
start_year <- min(as.numeric(substr(survey_circ_preprocess$survey_id, 4, 7)))

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
  k_dt              = k_dt,
  paed_age_cutoff   = paed_age_cutoff,
  rw_order          = rw_order,
  inc_time_tmc      = inc_time_tmc
)

#### Modelling circumcision probabilities ####

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