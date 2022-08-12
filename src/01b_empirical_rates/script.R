#### Pull Empirical Circumcision Rates ####

# library(dplyr, warn.conflicts = FALSE)
# library(sf)
# library(tidyr)
# devtools::load_all("../threemc")

dir_loc <- "depends/"
save_loc <- "artefacts/"
threemc::create_dirs_r(save_loc)

# age groups to aggregate to 
age_groups <- c(
  "0-4",   "5-9",   "10-14", "15-19",
  "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "54-59",
  "15-24", "10-24", "15-29", "10-29", 
  "15-39", "10-39", "15-49", "10-49"
)

# remove circumcisions with missing type?
rm_missing_type <- FALSE

k_dt <- 5 # Age knot spacing
start_year <-  2006
cens_age <- 59
N <- 1000

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)


#### Load Data ####

# modelling results
# results <- readr::read_csv(file.path(
#   dir_loc, 
#   # "01_modelling/20220621-152351-41844795/artefacts/Results_DistrictAgeTime_ByType.csv.gz"
#   "Results_DistrictAgeTime_ByType.csv.gz"
# ))
survey_circumcision <- readr::read_csv(file.path(
  dir_loc, 
  "survey_circumcision.csv.gz"
)) %>% 
  filter(iso3 == cntry)

# shapefiles
areas <- sf::read_sf(file.path(
  dir_loc, 
  # "00a2_areas_join/20220621-162124-d75bc170/artefacts/areas.geojson"
  "areas.geojson"
)) %>% 
  filter(iso3 == cntry)

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

# populations
populations <- readr::read_csv(file.path(
  dir_loc, 
  # "00c4_pops_aggregate/20220628-191440-ede1271e/artefacts/population_singleage_aggr.csv.gz"
  "population_singleage_aggr.csv.gz"
)) %>% 
  filter(iso3 == cntry)

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


#### Process Data #### 

# pull latest census year from survey_id
cens_year <- max(as.numeric(
  substr(unique(survey_circumcision$survey_id), 4, 7)
))

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
results <- threemc::prepare_survey_data(
  areas               = areas,
  survey_circumcision = select(survey_circumcision, -contains("area_level")),
  area_lev            = area_lev,
  start_year          = start_year,
  cens_age            = cens_age,
  cens_year           = cens_year,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
)

# Create a skeleton dataset for the area level of interest
results <- threemc::create_shell_dataset(
  survey_circumcision = results,
  population_data     = populations,
  areas               = areas,
  area_lev            = area_lev,
  time1               = "time1",
  time2               = "time2",
  strat               = "space",
  age                 = "age",
  circ                = "indweight_st"
)  %>% 
  filter(area_level == area_lev)

# "obs" cols give number of people who are circumcised in that 
# age/time/area stratum weighted by population 
# N is the total person years 

results <- results %>% 
  # calculate MC as MC + MMC + TMC
  mutate(
    obs_mc = case_when(
      !is.na(obs_mmc) & !is.na(obs_tmc) ~ obs_mc + obs_mmc + obs_tmc,
      !is.na(obs_mmc)                   ~ obs_mc + obs_mmc,
      !is.na(obs_tmc)                   ~ obs_mc + obs_tmc,
      TRUE                              ~ obs_mc
    )
  ) %>% 
  # pivot empirical person year columns to the one column
  # pivot_longer(cols = obs_mmc:icens, names_to = "type", values_to = "mean") %>% 
  pivot_longer(cols = obs_mmc:obs_mc, names_to = "type", values_to = "mean") %>% 
  # Only keep required columns 
  select(area_id:age, type, N, mean) %>% 
  mutate(
    # shouldn't be surveyed circumcisions for under 15s
    # don't need to do this! 
    # mean = ifelse(age < 15, 0, mean),
    # Calculate empirical rates
    # across(obs_mmc:icens, ~ ifelse(. == 0, 0, . / N))
    mean = ifelse(mean == 0 | N == 0, 0, mean / N)
  ) %>% 
  select(-N) %>% 
  # remove 0s, so taking log doesn't give -Inf
  mutate(mean = ifelse(mean == 0, NA_real_, mean))

# only keep relevant columns in populations for left_join
populations_append <- populations %>% 
  dplyr::select(
    names(results)[names(results) %in% names(populations)],
    population,
    # don't join by area_name, in case character encoding etc causes errors
    -dplyr::matches("area_name")
  )

# join in region populations
results <- dplyr::left_join(results, populations_append)

# Add parent areas
results <- threemc:::combine_areas(
  results, 
  areas_wide, 
  area_lev, 
  # add_keep_cols = c("obs_mmc", "obs_tmc", "obs_mc", "cens", "icens"),
  add_keep_cols = "mean",
  join = FALSE, 
  fill = TRUE
)

# ensure there is no duplication
results <- results %>% 
  bind_rows() %>% 
  distinct() %>% 
  group_split(area_level, .keep = TRUE)


#### Change Age to Age Group ####

# save aggregated single age results
results_single_age <- bind_rows(results) %>% 
  # ensure rows aren't doubled
  distinct() %>% 
  select(-c(space, circ_age, time)) %>% 
  # Don't think I should be doing this! These are individual obs, not to be summed!
  # group_by(area_id, area_name, year, age, type) %>% 
  # summarise(
  #   population = sum(population, na.rm = TRUE), 
  #   mean = sum(mean, na.rm = TRUE), .groups = "drop"
  # ) %>% 
  identity()

# Multiplying by population to population weight
results_list <- lapply(results, function(x) {
    x %>% 
    dplyr::mutate(mean = mean * population) %>% 
    relocate(mean, .after = everything())
})

# aggregate sample for each age group
results <- lapply(seq_along(age_groups), function(i) {
  # If upper limit use this split
  if (grepl("-", age_groups[i])) {
    age1 <- as.numeric(strsplit(age_groups[i], "-")[[1]][1])
    age2 <- as.numeric(strsplit(age_groups[i], "-")[[1]][2])
  }
  # If no upper limit use this split
  if (grepl("\\+", age_groups[i])) {
    age1 <- as.numeric(strsplit(age_groups[i], "\\+")[[1]][1])
    age2 <- Inf
  }
  results_list_loop <- lapply(results_list, function(x) {
    x <- x %>%
      distinct() %>% 
      # take results for age group i
      dplyr::filter(.data$age >= age1, .data$age <= age2) %>%
      dplyr::select(-.data$age)
    # Getting summarising samples
    x <- data.table::setDT(x)[,
                              lapply(.SD, sum, na.rm = T),
                              by = c("area_id", "area_name", "year", "type"),
                              # .SDcols = c("population", paste0("samp_", c(1:N)))
                              # .SDcols = c("population", "N")
                              .SDcols = c("population", "mean")
    ]
    # Adding age group
    dplyr::mutate(x, age_group = age_groups[i])
  })
  # Printing index
  print(age_groups[i])
  # return ages
  return(results_list_loop)
})

# join together
results <- bind_rows(rlang::squash(results))

# Multiplying by population to population weight
results <- results %>% 
  # dplyr::mutate(across(obs_mmc:icens, ~ . / population))
  dplyr::mutate(mean = mean / population)

#### Final ####

# Merge regional information on the dataset 
merge_empirical_rates <- function(.data) {
  threemc:::merge_area_info(.data, sf::st_drop_geometry(areas)) %>% 
    mutate(
      iso3 = substr(area_id, 0, 3),
      type = case_when(
        type == "obs_mc" ~ "MC probability",
        type == "obs_mmc" ~ "MMC probability",
        type == "obs_tmc" ~ "TMC probability"
      )  
    ) %>% 
    relocate(iso3) %>% 
    relocate(contains("age"), .after = year)
}

results <- merge_empirical_rates(results) 
results_single_age <- merge_empirical_rates(results_single_age)

# return results
readr::write_csv(
  results, 
  file.path(save_loc, "empirical_rates.csv.gz")
)
readr::write_csv(
  results_single_age, 
  file.path(save_loc, "empirical_rates_singleage.csv.gz")
)