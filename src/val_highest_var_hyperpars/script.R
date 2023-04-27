## Find Hyperpars that give the largest increase in projection period error ##

## Note: Need to repeat this script x3 for AR 1, RW 1 and RW 2 fits
## Actually: Could add temporal prior as argument to task!

#### Initial ####

# paths to dependencies and save locations
dir_path <- "depends/"
save_loc <- "artefacts/"

# ensure save loc exists
threemc::create_dirs_r(save_loc)

# which model should we look at? (could be better parameterised in previous tasks)
# Also could be changed into a nice switch statement
# if (rw_order == "AR 1") {
#   extra_pattern <- "AR"
# } else if (rw_order == 1) {
#   extra_pattern <- "RW_1"
# } else if (rw_order == 2) {
#   extra_pattern <- "RW 2"
# } else {
#   stop("Please provide valid rw_order argument (choose from one of 0, 1 or 2)")
# }


#### Metadata ####

spec_age_group <- "15-29"
forecast_year <- 2021

# countries with aggregations
cntries <- sort(c(
  "AGO", "BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", 
  "ETH", "GHA", "GIN", "GMB", "KEN", "LBR", "LSO", "MLI", 
  "MOZ", "MWI", "NAM", "NER", "NGA", "RWA", "SEN", "SLE", 
  "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE", "GAB"
  # "BWA", "CAF", "GNB", "GNQ" # countries not (yet) modelled
))


#### Load Data #### 

# function to load data
load_data <- function(pattern, dir_path = "depends/", exclude = NULL, extra_pattern = NULL) {
  locs <- sort(list.files(
    path = dir_path, pattern = pattern, full.names = TRUE
  ))
  
  # exclude certain files
  if (!is.null(exclude)) {
    locs <- locs[!grepl(exclude, locs)]
  }
  
  # additional pattern (allows choice of AR 1, RW 1 or RW 2)
  if (!is.null(extra_pattern)) {
    locs <- locs[grepl(extra_pattern, locs)]
  }
  
  if (grepl(".csv.gz", locs[1])) {
    dfs <- lapply(locs, readr::read_csv, show_col_types = FALSE)
  } else {
    dfs <- lapply(locs, readRDS)
  }
  return(dfs)
}

#### Function to find hyperparameters for each prior ####

find_optimal_hyperpars <- function(
    cntries, spec_age_group, forecast_year, dir_path, extra_pattern
  ) {
  
  #### Load data ####
  
  # load models 
  models <- load_data(".rds", extra_pattern = extra_pattern) 
  
  # # load aggregated prevalences
  fits <- load_data("Prevalence.csv.gz", extra_pattern = extra_pattern)
  
  # load surveys for each country
  surveys <- readr::read_csv(
    file.path(dir_path, "survey_circumcision.csv.gz"),
    show_col_types = FALSE
  )
  
  
  #### Pull hyperparameters from TMB objects ####
  
  # pull hyperparameters from each model
  hyperparameters <- lapply(models, "[[", "par")
  
  # coerce hyperparameters into dataframe
  hyperparameters_df <- tibble()
  
  # only take countries for which we have models 
  cntries <- cntries[cntries %in% unique(unlist(lapply(fits, `[[`, "area_id")))]
  
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
  
  largest_var_cntries_df <- fit_df %>% 
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
    arrange(desc(sd_change))
  
  largest_var_cntries <- largest_var_cntries_df %>% 
    slice(1:5) %>% 
    pull(iso3)
  
  hyperparameters_df_wide <- hyperparameters_df_wide %>% 
    left_join(largest_var_cntries_df) %>% 
    arrange(desc(sd_change)) %>% 
    # label with prior 
    mutate(prior = extra_pattern)
  
  target_hyperpars <- hyperparameters_df_wide %>% 
    filter(iso3 %in% largest_var_cntries)
  
  return(list(
    "hyperparameters_df_wide" = hyperparameters_df_wide,
    "target_hyperpars"        = target_hyperpars
  )) 
}

ar_1_hyperpars <- find_optimal_hyperpars(
  cntries, spec_age_group, forecast_year, dir_path, "AR"
)
rw_1_hyperpars <- find_optimal_hyperpars(
  cntries, spec_age_group, forecast_year, dir_path, "RW_1"
)
rw_2_hyperpars <- find_optimal_hyperpars(
  cntries, spec_age_group, forecast_year, dir_path, "RW_2"
)


#### Save outputs ####

readr::write_csv(
  ar_1_hyperpars$hyperparameters_df_wide,
  file.path(save_loc, "ar_var_corr_hyperpars.csv.gz")
)

readr::write_csv(
  ar_1_hyperpars$target_hyperpars, 
  file.path(save_loc, "ar_high_var_hyperpars.csv.gz")
)

readr::write_csv(
  rw_1_hyperpars$hyperparameters_df_wide,
  file.path(save_loc, "rw_1_var_corr_hyperpars.csv.gz")
)

readr::write_csv(
  rw_1_hyperpars$target_hyperpars, 
  file.path(save_loc, "rw_1_high_var_hyperpars.csv.gz")
)

readr::write_csv(
  rw_2_hyperpars$hyperparameters_df_wide,
  file.path(save_loc, "rw_2_var_corr_hyperpars.csv.gz")
)

readr::write_csv(
  rw_2_hyperpars$target_hyperpars, 
  file.path(save_loc, "rw_2_high_var_hyperpars.csv.gz")
)