#### Posterior Predictive Check (Prevalence) ####

#### Preliminaries ####

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

spec_age_groups <- c(
  "0-4", "5-9", "10-14", "15-19", 
  "20-24", "25-29", "30-34", "35-39", 
  "40-44", "45-49", "50-54", "54-59"
)


#### Preparing location/shapefile information ####

# load shapefile
areas <- read_circ_data(
  path    = "depends/areas.geojson",
  filters = c("iso3" = cntry)
  ) %>%
  # Add a unique identifier within Admin code and merging to boundaries
  sf::st_drop_geometry() %>%
  group_by(area_level) %>%
  mutate(space = row_number()) %>%
  ungroup()

# also take wide areas
areas_wide <- sf::st_drop_geometry(areas) %>% 
  dplyr::select(dplyr::all_of(
    c("area_id", "area_name", "parent_area_id", "area_level", "space")
  )) %>%
  spread_areas(space = FALSE)

# load populations
populations <- read_circ_data(
  "depends/population_singleage_aggr.csv.gz",
  filters = c("iso3" = cntry, "sex" = "male")
)

# Load survey points
survey_estimates <- read_circ_data(
  "depends/survey-circumcision-coverage.csv.gz"
) %>% 
  filter(iso3 == cntry) %>% 
  rename(
    year = survey_mid_calendar_quarter,
    mean = estimate,
    sd = std_error,
    lower = ci_lower,
    upper = ci_upper
  ) %>% 
  # change age group convention from YXXX_XXX to XX-XX
  change_agegroup_convention() %>% 
  # have indicator column match convention as well
  mutate(
    indicator = case_when(
      indicator == "circumcised"  ~ "MC",
      indicator == "circ_medical" ~ "MMC",
      TRUE                        ~ "TMC"
    )
  ) %>% 
  # only take desired age groups
  filter(age_group %in% spec_age_groups)

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

# Model with Probability of MC
out <- read_circ_data("depends/Results_DistrictAgeTime_ByType_OOS.csv.gz")

# "small" model fit object 
fit <- readRDS("depends/TMBObjects_DistrictAgeTime_ByType_OOS.rds")

# specify model, depending on whether there is an mmc/tmc split in results
if (all(out$obs_mmc == 0 & out$obs_tmc == 0)) {
  mod <- "Surv_SpaceAgeTime"
} else {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
}

# re-sample from model
if (is.null(fit$sample)) {
    fit <- threemc_fit_model(
        fit     = fit,
        mod     = mod,
        randoms = c("u_time_mmc", "u_age_mmc", "u_space_mmc",
                    "u_agetime_mmc", "u_agespace_mmc", "u_spacetime_mmc",
                    "u_age_tmc", "u_space_tmc", "u_agespace_tmc"),
        N       = N
    )
}

# load used survey information
used_survey_df <- readr::read_csv("depends/used_survey_info.csv")
# years for removed surveys from OOS validation
removed_years <- as.numeric(substr(used_survey_df$removed_surveys, 4, 7))


#### Joining Samples with Results ####

# filter to area level modelled 
out <- out %>%
  dplyr::filter(.data$area_level == area_lev)

# join in populations
out <- out %>% 
  left_join(select(populations, -c(area_name, area_level)))

# pull N samples for each desired output from fit
samples <- fit$sample
# only want "cum_inc" or rates 
samples <- samples[grepl("cum_inc", names(samples))]
# ensure names for MC columns in fit have the suffix "_mc"
samples <- threemc:::append_mc_name(samples)
# replace "cum_inc"
names(samples) <- toupper(stringr::str_replace_all(names(samples), "cum_inc_", ""))


#### Split out between types ####

# split between following types
types_out <- c("MC", "MMC", "TMC")

# select identification cols & rate col of interest and join with samples
out_types <- lapply(types_out, function(x) {
  out_spec <- select(out, area_id:population) # do I need pop? Maybe if aggregating
  n <- length(out_spec)
  out_spec[, (n +1):(n + N)] <- samples[[x]]
  out_spec <- out_spec %>% 
    mutate(indicator = x)
  # filter(out_spec, year %in% removed_years)
}) %>% 
  bind_rows() %>% 
  # only take years of interest, in which surveys were removed from model data
  filter(year %in% removed_years, area_level == area_lev) # also modelled lev


#### Aggregate to Age Groups #### 

# change col names to work in aggregate_sample_age_group
# out_types <- lapply(out_types, function(x) {
#   names(x)[grepl("V", names(x))] <- paste0("samp_", 1:N)
#   return(rename(x, type = indicator))
# })

names(out_types)[grepl("V", names(out_types))] <- paste0("samp_", 1:N)
out_types <- rename(out_types, type = indicator)

out_types_agegroup <- threemc:::aggregate_sample_age_group(
  list(out_types),
  aggr_cols = c(
    "area_id", "area_name", "area_level", "year", "type"
  ), 
  age_groups = spec_age_groups, 
  N = N
) %>% 
  rename(indicator = type) %>% # rename to match survey points df
  relocate(age_group, .before = samp_1) %>% # have sample cols last 
  filter(area_level == 1) # filter for area level 1 only


#### Prepare Survey Points & Join with Samples ####

survey_estimates_prep <- survey_estimates %>% 
  # filter for OOS year(s) and modelled area_level 
  # TODO: Could also aggregate up to other area levels for out_types_agegroup??
  filter(
    year %in% removed_years, 
    area_level == 1,
    age_group %in% out_types_agegroup$age_group,
    mean != 0
  ) %>% 
  # ignore survey_id, merging for the same year
  group_by(area_id, year, age_group, indicator) %>% 
  summarise(mean = sum(mean), .groups = "drop") 

# join with samples
survey_estimates_ppd <- survey_estimates_prep %>% 
  left_join(out_types_agegroup)

# save for later analysis
readr::write_csv(survey_estimates_ppd, file.path(save_dir, "ppd.csv.gz"))


#### Calculating Posterior Predictive Check for Prevalence Estimations ####

# function to calculate where in model sample prevalence dist empirical rates are
quant_pos_sum <- function(y, x) if (y < x) 0 else 1
 
survey_estimates_ppd_dist <- survey_estimates_ppd %>% 
  relocate(mean, .before = samp_1) %>%
  group_by(across(area_id:area_level)) %>%
  rowwise() %>%
 summarise(
  quant_pos = sum(across(starts_with("samp_"), ~quant_pos_sum(
    mean, .x
  ))),
  .groups = "drop"
 )

# calculate percentage of samples within 95% CI from PPD
x <- survey_estimates_ppd_dist$quant_pos
x <- sum(x >= 0.025 * N & x <= 0.975 * N) / length(x)
print(paste0(
  "Percentage of survey points which fall within posterior predictive",
  " distribution 95% CI: ", 
  round(x * 100, 2), 
  "%"
))

# also save
readr::write_csv(
  survey_estimates_ppd_dist, 
  file.path(save_dir, "ppd_quant_pos.csv.gz")
)
