#### Pull Empirical Circumcision Rates ####

dir_loc <- "depends/"
save_loc <- "artefacts/"
threemc::create_dirs_r(save_loc)

# Age groups to aggregate to:
# 5-year age groups from 0 to 60
five_year_age_groups <- c(
  "0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "54-59"
)

# other, wider age groups of interest
add_age_groups <- c(
  # no caps
  "0+", "10+", "15+",
  # others
  "15-24", "10-24", "15-29", "10-29",
  "15-39", "10-39", "15-49", "10-49"
)

age_groups <- c(five_year_age_groups, add_age_groups)

# remove circumcisions with missing type?
rm_missing_type <- FALSE

k_dt <- 5 # Age knot spacing
start_year <-  2002
forecast_year <- 2021
cens_age <- 59

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)


#### Load Data ####

# load surveys
survey_circumcision <- readr::read_csv(file.path(
  dir_loc, "survey_circumcision.csv.gz"
))
# load populations
populations <- readr::read_csv(file.path(
  dir_loc, "population_singleage_aggr.csv.gz"
))

# load shapefiles
areas <- sf::read_sf(file.path(
  dir_loc, "areas.geojson"
))
# wide formatted areas
areas_wide <- sf::st_drop_geometry(areas) %>% 
  dplyr::group_by(.data$area_level) %>%
  dplyr::mutate(space = dplyr::row_number()) %>%
  dplyr::ungroup() %>% 
  dplyr::select(dplyr::all_of(
    c("iso3", "area_id", "area_name", "parent_area_id", "area_level", "space")
  )) %>%
  group_split(iso3) %>% 
  purrr::map(~ spread_areas(areas = .x, space = FALSE)) %>% 
  bind_rows()


#### Process Data #### 

# pull latest and first censoring year from survey_id
survey_years <- as.numeric(substr(unique(survey_circumcision$survey_id), 4, 7))
start_year <- max(min(survey_years), start_year) # have lower bound on start

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circumcision_preprocessed <- prepare_survey_data(
  areas               = areas,
  survey_circumcision = select(survey_circumcision, -contains("area_level")),
  area_lev            = threemc::datapack_psnu_area_level,
  start_year          = start_year,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
)

# Create shell datasets for each country
out_list <- lapply(survey_circumcision_preprocessed, function(x) {
  
  print(x$iso3[1])
  
  create_shell_dataset(
    survey_circumcision = x,
    populations         = populations,
    areas               = areas,
    area_lev            = max(x$area_level),
    start_year          = start_year,
    end_year            = forecast_year,
    time1               = "time1",
    time2               = "time2",
    strat               = "space",
    age                 = "age",
    circ                = "indweight_st"
  ) %>% 
    filter(area_level == max(x$area_level))
})

#### Calculate Empirical Rates from Shell Datasets ####

# calculate empirical rates from these shell datasets
empirical_rates_list <- lapply(out_list, function(x) {
  threemc_empirical_rates(x, areas, max(x$area_level), populations, age_groups)
})

# return results
readr::write_csv(
  bind_rows(empirical_rates_list), 
  file.path(save_loc, "empirical_rates.csv.gz")
)
# readr::write_csv(
#   results_single_age, 
#   file.path(save_loc, "empirical_rates_singleage.csv.gz")
# )
