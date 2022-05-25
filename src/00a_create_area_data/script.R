#### Area hierarchy ####

# notes:
# - is HTI needed?
# - areas created with this code does not match areas used in modelling analysis
# - actually missing a lot here lol, look back at `create-data.R`
# - Missing ZAF, MWI, survey_individuals data from global

# countries to pull shapefiles for 
iso3 <- c("ago", "bdi", "ben", "bfa", "bwa", "caf", "civ", "cmr", "cod",
          "cog", "eth", "gab", "gha", "gin", "gmb", "gnb", "gnq", "hti",
          "ken", "lbr", "lso", "mli", "moz", "mwi", "nam", "ner", "nga",
          "rwa", "sen", "sle", "swz", "tcd", "tgo", "tza", "uga", "zaf",
          "zmb", "zwe")
# append iso3 code with name of areas files for each country
area_paths <- paste0("depends/", iso3, "_areas.geojson")
names(area_paths) <- toupper(iso3)
# replace MWI areas with TA-level for linking survey data
area_paths["MWI"] = "depends/mwi_areas_ta.geojson"

# load shapefiles
areas <- lapply(area_paths, read_circ_data)

# Clean GMB area hierarchy geometry. (This should be done way upsteam)
areas[["GMB"]] <- sf::st_collection_extract(areas[["GMB"]], "POLYGON")

# make sure to add iso3 column to start of each area file
# also only keep required columns (AGO known to have correct cols)
keep_cols <- unique(c("iso3", colnames(areas[["AGO"]])))
areas <- lapply(seq_along(areas), function(i) {
  areas[[i]]$iso3 <- names(areas)[i]
  # return(select(areas[[i]], all_of(keep_cols)))
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
sf::write_sf(areas, "artefacts/areas.geojson")