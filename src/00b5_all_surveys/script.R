#### Parsing Survey Information ####

#### Preliminaries, Metadata ####

# dependencies and save locations
dir_path <- "depends/"
save_loc <- "artefacts/"
threemc::create_dirs_r(save_loc)

k_dt <- 5 # Age knot spacing
start_year <-  2000
cens_age <- 59

sf_use_s2(FALSE)


#### Load Data ####

# read in shapefiles and survey data, filter for male surveys only
filters <- c(sex = "male")
areas <- read_circ_data(paste0(dir_path, "areas.geojson"), filters) %>%
  # add space column to areas for each area level
  group_by(iso3, area_level) %>% 
  dplyr::mutate(space = 1:dplyr::n()) %>%  
  ungroup()
survey_circumcision <- read_circ_data(
  paste0(dir_path, "survey_circumcision.csv.gz"), filters
)

#### Prepare Circumcision Data ####

# Prepare circ data, normalise survey weights and apply Kish coefficients.
survey_circ_preprocess <- prepare_survey_data(
    areas               = areas,
    survey_circumcision = survey_circumcision,
    area_lev            = threemc::datapack_psnu_area_level,
    start_year          = start_year,
    cens_age            = cens_age,
    rm_missing_type     = FALSE,
    norm_kisk_weights   = TRUE
)
survey_circ_preprocess <- data.table::rbindlist(
    survey_circ_preprocess, fill = TRUE
)

#### Find Last Surveys ####

# Find the last survey for each country
# If a countries next most recent survey is the year before, also keep that
# These are the surveys that will be removed during OOS investigations
last_surveys <- survey_circ_preprocess %>%
  filter(area_id != iso3) %>% # will calculate last country survey later
  select(iso3, area_id, survey_year = year) %>% 
  group_by(iso3, area_id) %>%
  mutate(max_year = max(survey_year)) %>% 
  ungroup() %>% 
  filter(survey_year %in% c(max_year, max_year - 1)) %>% 
  select(-max_year)
 
# find for each county, as well as at district level
cntry_last_surveys <- survey_circ_preprocess %>%
  select(iso3, survey_year = year)

cntry_max_survey_years <- cntry_last_surveys %>% 
  group_by(iso3) %>%
  summarise(max_year = max(survey_year), .groups = "drop")
cntry_max_survey_years <- bind_rows(
  cntry_max_survey_years,
  mutate(cntry_max_survey_years, max_year = max_year - 1)
)

cntry_last_surveys <- cntry_last_surveys %>%
  semi_join(
    cntry_max_survey_years, 
    by = c("iso3", "survey_year" = "max_year")
  ) %>% 
  distinct() %>% 
  mutate(area_id = iso3)
 
last_surveys <- bind_rows(last_surveys, cntry_last_surveys) %>%
    arrange(iso3, area_id)


#### Find All Surveys ####

# also save all surveys
all_surveys <- survey_circ_preprocess %>%
    distinct(iso3, area_id, survey_year = year) %>%
    arrange(iso3, area_id, survey_year)

cntry_all_surveys <- survey_circ_preprocess %>%
    group_by(iso3) %>%
    distinct(survey_year = year) %>%
    ungroup() %>%
    mutate(area_id = iso3)

all_surveys <- bind_rows(all_surveys, cntry_all_surveys) %>%
    arrange(iso3, area_id)

#### Save ####

fwrite(last_surveys, file.path(save_loc, "last_surveys.csv"))
fwrite(all_surveys, file.path(save_loc, "all_surveys.csv"))