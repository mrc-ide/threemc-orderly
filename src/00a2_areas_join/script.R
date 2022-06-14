#### Area hierarchy ####

# ensure save directory exits
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir)

# countries to pull shapefiles for 
iso3 <- c("ago", "bdi", "ben", "bfa", "bwa", "caf", "civ", "cmr", "cod",
          "cog", "eth", "gab", "gha", "gin", "gmb", "gnb", "gnq", "ken", 
          "lbr", "lso", "mli", "moz", "mwi", "nam", "ner", "nga", "rwa", 
          "sen", "sle", "swz", "tcd", "tgo", "tza", "uga", "zaf", "zmb", "zwe")

# append iso3 code with name of areas files for each country
area_paths <- paste0("depends/", iso3, "_areas.geojson")
names(area_paths) <- toupper(iso3)

# load shapefiles
areas <- lapply(area_paths, sf::read_sf)

# make sure to add iso3 column to start of each area file
# also only keep required columns (AGO known to have correct cols)
# keep_cols <- unique(c("iso3", colnames(areas[["AGO"]])))
areas <- lapply(seq_along(areas), function(i) {
  areas[[i]]$iso3 <- names(areas)[i]
  # return(select(areas[[i]], all_of(keep_cols)))
  # return(select(areas[[i]], -(contains("level") & !matches("area_level"))))
  return(select(areas[[i]], -(contains("level") & !matches("area_level"))))
})

# bind area files together
areas <- bind_rows(areas) %>% 
  select(iso3, everything(), -matches("n_areas"))

# not the same as this for some reason!! 
# areas_test <- sf::read_sf("~/OneDrive/data/areas.geojson")
# anti_join(sf::st_drop_geometry(areas) %>% arrange(area_id, area_name, area_sort_order), 
#           sf::st_drop_geometry(areas_test) %>% arrange(area_id, area_name, area_sort_order)) %>%
#   distinct(iso3) %>% 
#   pull()

# save data
sf::write_sf(areas, paste0(save_dir, "areas.geojson"))