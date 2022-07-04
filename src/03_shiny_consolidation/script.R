## Consolidate Data for Shiny plots from modelling output ##

#### Initial ####

# paths to dependencies and save locations
dir_path <- "depends/"
save_loc <- "artefacts/"

# ensure save loc exists
threemc::create_dirs_r(save_loc)

# Specify orderly_root
orderly_root <- here::here()
orderly_root <- unlist(stringr::str_split(orderly_root, "/"))
orderly_root <- orderly_root[1:which(orderly_root == "threemc-orderly")]
orderly_root <- paste(orderly_root, collapse = "/")

# shapefile
areas <- sf::read_sf(paste0(dir_path, "areas.geojson")) %>% 
  group_by(area_level) %>%
  dplyr::mutate(space = row_number()) %>%
  ungroup()


#### Single Plots #### 

# load and save single age and age group results
agg_results_saver <- function(spec_type, dir_path, save_loc = NULL) {
  
  # files location
  files <- list.files(dir_path,full.names = TRUE)
  files <- files[grepl(toupper(paste0(spec_type, "_")), toupper(files))]
  
  # load results
  message("loading ...")
  results <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE))
  # results <- lapply(files, readr::read_csv, show_col_types = FALSE)
  # results <- bind_rows(lapply(results, order_area_name, areas = areas))
  
  if (!"iso3" %in% names(results)) {
    results$iso3 <- substr(results$area_id, 0, 3)
  }
  
  # only keep "types" needed for plots
  no_prog_types <- c("MC", "MMC", "TMC")
  spec_types <- c(
    paste0(c(paste0(no_prog_types, "s"), "MMC-nTs", "TMICs"), " performed"),
    paste0(no_prog_types, " coverage"),
    paste0(no_prog_types, " probability")
  )
  results <- results %>% 
    filter(type %in% spec_types)
  
  # save 
  if (!is.null(save_loc)) {
    message("saving ...")
    readr::write_csv( 
      x = results, 
      file = paste0(save_loc, "results_", spec_type, ".csv.gz")
    )
  } else {
    return(results)
  }
}

# results_age <- agg_results_saver("age", dir_path, areas)
results_age <- agg_results_saver("age", dir_path)
readr::write_csv( 
  x = results_age,
  file = paste0(save_loc, "results_age.csv.gz")
)
rm(results_age); gc()

# results_agegroup <- agg_results_saver("agegroup", dir_path, areas)
results_agegroup <- agg_results_saver("agegroup", dir_path)
readr::write_csv( 
  x = results_agegroup, 
  file = paste0(save_loc, "results_agegroup.csv.gz")
)

#### Comparison Plots ####

# areas for surveys and dmppt2 Shiny data
areas_join <- areas %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    iso3,       area_id,          area_name,
    area_level, area_level_label, area_sort_order
  ) %>%
  distinct()
# readr::write_csv(
#   areas_join,
#   paste0(dir_path, "areas_join.csv.gz")
# )

# results_agegroup for comparison plots
results_agegroup <- results_agegroup %>% 
  filter(grepl("coverage", type)) %>% 
  select(-area_name) %>% 
  left_join(areas_join)
 
readr::write_csv(
  results_agegroup,
  paste0(save_loc, "results_agegroup_comparison.csv.gz")
)

# only keep areas in results_agegroup in below
spec_areas <- unique(results_agegroup$area_id)

rm(results_agegroup); gc()


# DMPPT2 data
dmppt2_data <- readr::read_csv(
  paste0(dir_path, "dmppt2-2021_circumcision_coverage.csv.gz")
)

# survey_data
survey_data <- read_circ_data(
  paste0(dir_path, "survey-circumcision-coverage.csv.gz"),
  filters = c("sex" = "male")
)

# change naming convention of survey data (not working currently!)
if ("survey_mid_calendar_quarter" %in% names(survey_data)) {
  survey_data <- survey_data %>%
    rename(
      year = survey_mid_calendar_quarter,
      mean = estimate,
      sd = std_error,
      lower = ci_lower,
      upper = ci_upper
    )
}

# order by area
comparison_plots_cleaner <- function(.data, areas_join, spec_areas = NULL) {
  .data <- left_join(select(.data, -area_name), areas_join) 
  if (!is.null(spec_areas)) .data <- filter(.data, area_id %in% spec_areas)
  
  # .data <- group_split(.data, iso3)
  
  # order by area_id temporarily, rather than with order_area_name
  # .data <- lapply(.data, order_area_name)
  # .data <- arrange(bind_rows(.data), area_id)
  .data <- arrange(.data, area_id)
  
  # convert age group to our convention 
  # .data <- lapply(.data, change_agegroup_convention)
  .data <- change_agegroup_convention(.data)
  
  # bind rows and return
  # return(bind_rows(.data))
  return(.data)
}

survey_data <- comparison_plots_cleaner(survey_data, areas_join, spec_areas)
readr::write_csv(
  survey_data, 
  paste0(save_loc, "survey-circumcision-coverage_shiny.csv.gz")
)
dmppt2_data <- comparison_plots_cleaner(dmppt2_data, areas_join, spec_areas)
readr::write_csv(
  dmppt2_data, 
  paste0(save_loc, "dmppt2-2021_circumcision_coverage_shiny.csv.gz")
)