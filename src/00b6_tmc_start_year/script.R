#### Initial ####

# metadata
start_year <- 1940
cens_age <- 59
cens_year <- 2020
min_start_year <- 1960
max_start_year <- 1998

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

# load data
areas <- read_circ_data("depends/areas.geojson") %>% 
  # add space column to areas
  group_by(iso3) %>% 
  dplyr::mutate(space = 1:dplyr::n()) %>% 
  ungroup()
areas <- st_make_valid(areas) 
survey_circumcision <- read_circ_data("depends/survey_circumcision.csv.gz")


#### Preprocess surveys for each country ####

survey_circ_preprocess <- bind_rows(prepare_survey_data(
  areas               = areas,
  survey_circumcision = survey_circumcision,
  # area_lev            = area_lev,
  area_lev            = max(areas$area_level),
  start_year          = start_year,
  cens_year           = cens_year,
  cens_age            = cens_age,
  norm_kisk_weights   = TRUE
))


#### Pull start dates ####

# take 1st of 1st 2 consecutive years with >= 0.01 of surveyed TMCs
tmc_circ_years <- survey_circ_preprocess %>% 
  filter(type == "TMC") %>% 
  group_split(iso3, .keep = TRUE) %>% 
  purrr::map(function(x) {
    janitor::tabyl(x, yoc)
  })
tmc_cntries <- unique(
  survey_circ_preprocess[survey_circ_preprocess$type == "TMC", ]$iso3
)

tmc_circ_years <- bind_rows(lapply(seq_along(tmc_circ_years), function(i) {
  as_tibble(tmc_circ_years[[i]]) %>% 
    mutate(iso3 = tmc_cntries[i]) %>% 
    relocate(iso3)
}))

start_dates <- tmc_circ_years %>% 
  filter(percent >= 0.01) %>% 
  group_by(iso3) %>% 
  slice(1) %>% 
  rename(year = yoc) %>% 
  arrange(year)

# add minimum and maximum start years
start_dates_final <- start_dates %>% 
  mutate(year = case_when(
    year < min_start_year ~ min_start_year,
    year > max_start_year ~ max_start_year, 
    TRUE                  ~ year
  ))

# Changes for some countries:
# may also have to change start dates for CMR, ZMB, BEN, NGA, GAB
start_dates_final <-  start_dates %>% 
  mutate(year = case_when(
    iso3 == "ETH" ~ 1980,
    iso3 == "TZA" ~ 1982,
    iso3 == "MWI" ~ 1978,
    iso3 == "UGA" ~ 1976,
    iso3 == "MOZ" ~ 1972,
    iso3 == "GHA" ~ 1980, 
    TRUE          ~ year
  ))

# save
readr::write_csv(
  start_dates_final, 
  paste0(save_dir, "tmc_start_dates.csv")
)
