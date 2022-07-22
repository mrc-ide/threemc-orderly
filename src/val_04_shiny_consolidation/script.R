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

oos_val_files <- list.files("depends/", full.names = TRUE)
oos_val_files <- oos_val_files[grepl("AgeGroup", oos_val_files)]
results_oos_val <- bind_rows(
  lapply(oos_val_files, readr::read_csv,  show_col_types = FALSE)
)

# add in required columns from areas and order area names
areas_join <- sf::st_drop_geometry(areas) %>%
  dplyr::select(
    iso3,       area_id,          area_name,
    area_level, area_level_label, area_sort_order
  ) %>%
  distinct()
results_oos_val <- results_oos_val %>% 
  select(-c(area_name, area_level)) %>% 
  left_join(areas_join)

# order area names 
results_oos_val <- order_area_name(results_oos_val)

# remove duplicates (shouldn't be there at all!)
results_oos_val <- results_oos_val %>%
  # group_by(area_id, year, model, type, age_group) %>% # too slow!!
  # filter(mean == max(mean, na.rm = TRUE)) %>%
  # ungroup() %>% 
  distinct()

readr::write_csv(
  x = results_oos_val,
  file = paste0(save_loc, "results_oos.csv.gz")
)
rm(results_oos_val); gc()
