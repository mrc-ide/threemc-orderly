## Find Hyperpars that give the largest increase in projection period error ##

#### Initial ####

# paths to dependencies and save locations
dir_path <- "depends/"
save_loc <- "artefacts/"

# ensure save loc exists
threemc::create_dirs_r(save_loc)


#### Metadata ####

spec_age_group <- "15-29"
forecast_year <- 2021

# countries with aggregations
cntries <- sort(c(
  "AGO", "BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", 
  "ETH", "GHA", "GIN", "GMB", "KEN", "LBR", "LSO", "MLI", 
  "MOZ", "MWI", "NAM", "NER", "NGA", "RWA", "SEN", "SLE", 
  "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
  # "BWA", "CAF", "GAB", "GNB", "GNQ" # countries not (yet) modelled
))


#### Load Data #### 

# function to load data
load_data <- function(pattern, dir_path = "depends/") {
  locs <- sort(list.files(
    path = dir_path, pattern = pattern, full.names = TRUE
  ))
  if (grepl(".csv.gz", locs[1])) {
    dfs <- lapply(locs, readr::read_csv, show_col_types = FALSE)
  } else {
    dfs <- lapply(locs, readRDS)
  }
  return(dfs)
}

# load models 
models <- load_data(".rds")

# load aggregated prevalences
fits <- load_data("Prevalence.csv.gz")

# load surveys for each country
surveys <- readr::read_csv(file.path(dir_path, "survey_circumcision.csv.gz"))


#### Pull hyperparameters from TMB objects ####

# pull hyperparameters from each model
hyperparameters <- lapply(models, "[[", "par")

# coerce hyperparameters into dataframe
hyperparameters_df <- tibble()
for (i in seq_along(cntries)) {
  spec_hyperparameters_df <- tibble("iso3" = cntries[i],
                                    "par" = names(hyperparameters[[i]]),
                                    "value" = hyperparameters[[i]])
  hyperparameters_df <- rbind(hyperparameters_df, spec_hyperparameters_df)
}
hyperparameters_df_wide <- hyperparameters_df %>%
  # pull mmc time variance and correlation hyperparameters
  filter(
    grepl("time", par) & grepl("mmc", par) & 
      (grepl("logsigma", par) | 
         grepl("logitrho", par))
) %>% 
  # convert to wide format
  tidyr::pivot_wider(names_from = "par", values_from = "value")


#### Find Projection Period for each country ####

# find last survey year for each country
last_survey_year_df <- surveys %>% 
  mutate(survey_year = as.numeric(substr(survey_id, 4, 7))) %>% 
  distinct(iso3, survey_year) %>% 
  group_by(iso3) %>% 
  filter(survey_year == max(survey_year))

# find projection period for each country
projection_years_df <- tidyr::crossing(
  "iso3" = cntries, 
  "year" = 2000:forecast_year
) %>% 
  left_join(last_survey_year_df, by = "iso3") %>% 
  filter(year >= survey_year) %>% 
  select(iso3, year)
  

####  Finding largest increases in error in projection ####

fit_df <- bind_rows(fits)

largest_var_cntries <- fit_df %>% 
  # take MMC coverage change at national level and specified age group
  filter(
    type ==  "MMC coverage", 
    age_group == spec_age_group,
    area_level == 0,
    area_id %in% hyperparameters_df_wide$iso3 # only countries with mmc pars
    ) %>%
  # take projection years only
  semi_join(projection_years_df, by = c("area_id" = "iso3", "year")) %>% 
  select(iso3 = area_id, year, sd) %>% 
  arrange(iso3, year) %>% 
  # calculate relative change in sd
  group_by(iso3) %>% 
  mutate(sd_change = (sd - lag(sd)) / sd) %>% 
  summarise(sd_change = max(sd_change, na.rm = TRUE), .groups = "drop") %>% 
  filter(!is.infinite(sd_change)) %>% 
  arrange(desc(sd_change)) %>% 
  slice(1:5) %>% 
  pull(iso3)

#### Pull Hyperparameters with ? ####

target_hyperpars <- hyperparameters_df_wide %>% 
  filter(iso3 %in% largest_var_cntries)

readr::write_csv(
  target_hyperpars, 
  file.path(save_loc, "high_var_hyperpars.csv.gz")
)

print(target_hyperpars)