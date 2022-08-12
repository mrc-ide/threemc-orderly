## Consolidate Data for Validation Shiny Module ##

#### Initial ####

# paths to dependencies and save locations
dir_path <- "depends/"
save_loc <- "artefacts/"

# ensure save loc exists
threemc::create_dirs_r(save_loc)

# shapefile
areas <- sf::read_sf(paste0(dir_path, "areas.geojson")) %>% 
  group_by(area_level) %>%
  dplyr::mutate(space = row_number()) %>%
  ungroup()

#### Pull & Save Data ####

files <- list.files("depends/", full.names = TRUE)

# shapefile information to join in
areas_join <- sf::st_drop_geometry(areas) %>%
  dplyr::select(
    iso3,       area_id,          area_name,
    area_level, area_level_label, area_sort_order
  ) %>%
  distinct()

data_preprocess <- function(files, areas_join, pattern, fixed = TRUE) {
  # oos_val_files <- files[grepl("OOS.", files, fixed = TRUE)]
  files <- files[grepl(pattern, files, fixed = fixed)]
  results <- bind_rows(
    lapply(files, readr::read_csv,  show_col_types = FALSE)
  )
  
  # join in shapefile information 
  results <- results %>% 
    select(-c(area_name, area_level)) %>% 
    left_join(areas_join)

  # order area names 
  results <- order_area_name(results)

  # remove duplicates (shouldn't be there at all!)
  results <- distinct(results)
}

results_oos_val <- data_preprocess(files, areas_join, "_OOS.")
results_investigating_var_corr <- data_preprocess(files, areas_join, "_test.")
results_oos_val_var_corr <- data_preprocess(files, areas_join, "test&OOS.")


readr::write_csv(
  x = results_oos_val,
  file = paste0(save_loc, "results_oos.csv.gz")
)
rm(results_oos_val); gc()
readr::write_csv(
  x = results_investigating_var_corr,
  file = paste0(save_loc, "results_investigating_var.csv.gz")
)
rm(results_investigating_var_corr); gc()
readr::write_csv(
  x = results_oos_val_var_corr,
  file = paste0(save_loc, "results_oos_var.csv.gz")
)
rm(results_oos_val_var_corr); gc()

survey_files <- files[grepl("used_survey", files)]
all_surveys <- bind_rows(
  lapply(survey_files, readr::read_csv,  show_col_types = FALSE)
)
# all_surveys <- stringr::str_split(all_surveys$used_surveys, ", ")
all_surveys <- unlist(rlang::squash(stringr::str_split(all_surveys$used_surveys, ", ")))
all_surveys_df <- data.frame(
  "survey_year" = as.numeric(substr(all_surveys, 4, 7))
)
all_surveys_df$iso3 <- all_surveys_df$area_id <- substr(all_surveys, 0, 3)
readr::write_csv(
  x = select(all_surveys_df, iso3, area_id, survey_year),
  file = paste0(save_loc, "used_surveys.csv.gz")
)