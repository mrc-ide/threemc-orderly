#### Initial ####

### Metadata to run the models

# !! Change this to use dataset stored in threemc
k_dt_age <- 5 # Age knot spacing
# if (!is.numeric(k_dt_time)) k_dt_time <- NULL # time knot spacing
k_dt_time <- NULL
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
N <- 1000
forecast_year <- 2021
paed_age_cutoff <- 10
# five-year age groups to perform posterior predictive checks for
five_year_age_groups <- c(
  "0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "54-59"
)
CI_range <- c(0.5, 0.8, 0.95) # Confidence intervals to find for PPD 

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

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
if (!is.na(area_lev) && !is.numeric(area_lev)) {
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
}
print(paste("area_level used is", area_lev))

# remove most recent survey 
# if most recent surveys are one year apart, remove both
# survey_years <- unique(survey_circumcision$survey_year)
# if (length(survey_years) < 2) stop("Too few survey years")
# second_last_year <- survey_years[length(survey_years) - 1]
# if (last(survey_years) - second_last_year == 1) {
#   removed_years <- c(second_last_year, last(survey_years))
# } else {
#   removed_years <- last(survey_years)
# }

survey_circumcision_test <- survey_circumcision

#### Load model data ####

fit <- readRDS("depends/TMBObjects_DistrictAgeTime_ByType.rds")
out_spec <- readr::read_csv("depends/Results_DistrictAgeTime_ByType.csv.gz")

# add populations to out
out_spec <- out_spec %>% 
  # filter(area_level == area_lev) %>% 
  left_join(select(populations, area_id, year, age, population)) 

# resample from fit
if (!"sample" %in% names(fit)) {
  set.seed(123)
  fit <- threemc_fit_model(
    fit           = fit,
    randoms       = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc",
      "u_time_tmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
    ),
    N             = N, 
    inner.control = list(maxit = 250)
  )
}

#### Perform Posterior Predictive Checks on original models ####

# ppc <- threemc_ppc(
#   fit,
#   out_spec,
#   survey_circumcision_test,
#   areas, 
#   area_lev, 
#   type = "MMC",
#   age_groups = five_year_age_groups,
#   CI_range = CI_range,
#   N = N
# )
types <- c("MMC", "TMC", "MC")
if (!any(grepl("mmc", tolower(names(fit$par))))) {
  types <- "MC"
}
print(paste0("types: ", paste(types, collapse = ", ")))

ppc_list <- lapply(types, function(x) {
  gc()
  return(threemc_ppc(
    fit,
    out_spec,
    survey_circumcision_test,
    areas, 
    area_lev, 
    type = x,
    age_groups = five_year_age_groups,
    CI_range = CI_range,
    N = N
  ))
})
gc()
names(ppc_list) <- types

#### Save results ####

# Save results
# data.table::fwrite(
#   proposal_mod$out, file = paste0(save_dir, "Results_DistrictAgeTime_ByType.csv.gz")
# )
# 
# # save fit as .rds file
# saveRDS(proposal_mod$fit, paste0(save_dir, "TMBObjects_DistrictAgeTime_ByType.rds"))

# save ppc df 
data.table::fwrite(
  # proposal_mod$ppc$ppc_df, 
  rbindlist(lapply(ppc_list, `[[`, "ppc_df")),
  file = file.path(save_dir, "pointwise_ppc_df.csv.gz")
)

# save summarised ppc as .rds file
saveRDS(
  # proposal_mod$ppc$summary_stats, 
  lapply(ppc_list, `[[`, "summary_stats"),
  file.path(save_dir, "ppc_summary.rds")
)
