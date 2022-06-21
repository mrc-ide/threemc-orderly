#### Area hierarchy ####

# notes:
# - areas created with this code does not match areas used in modelling analysis
# - actually missing a lot here lol, look back at `create-data.R`

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
# replace MWI areas with TA-level for linking survey data
area_paths["MWI"] = "depends/mwi_areas_ta.geojson"

# load shapefiles
areas <- lapply(area_paths, read_circ_data)

# Clean GMB area hierarchy geometry. (This should be done way upsteam)
areas[["GMB"]] <- sf::st_collection_extract(areas[["GMB"]], "POLYGON")

# save files 
save_locs <- paste0(save_dir, iso3, "_areas.geojson")
lapply(seq_along(save_locs), function(i) {
  sf::write_sf(areas[[i]], save_locs[i])
})

# save record of IDs used in current task
ids_used <- dplyr::tribble(
  ~"task",          ~"id",
  "ago_data_areas", "20220518-152244-6423130e",
  "bdi_data_areas", "20220518-155031-215356d9",
  "ben_data_areas", "20220526-161853-b5460ca3",
  "bfa_data_areas", "20220516-145628-0a2334a1",
  "bwa_data_areas", "20220518-144729-c8894295",
  "caf_data_areas", "20220518-145504-892cf80a",
  "civ_data_areas", "20220526-110942-64a9ac61",
  "cmr_data_areas", "20220516-145843-7441a4c9",
  "cod_data_areas", "20220518-161132-955fed32",
  "cog_data_areas", "20220519-153623-d23142d0",
  "eth_data_areas", "20220518-162235-06985266",
  "gab_data_areas", "20220516-145926-30cde1c9",
  "gha_data_areas", "20220519-153739-e2a13cbb",
  "gin_data_areas", "20220516-145944-32ae32a2",
  "gmb_data_areas", "20220516-145949-5cd9cc1e",
  "gnb_data_areas", "20220519-153723-8de28967",
  "gnq_data_areas", "20220516-145954-a19b6747",
  "ken_data_areas", "20220526-144025-ba370a37",
  "lbr_data_areas", "20220516-150038-e90c6fd8",
  "lso_data_areas", "20220519-134431-50305894",
  "mli_data_areas", "20220519-153836-e539828f",
  "moz_data_areas", "20220613-143914-279bc764",
  "nam_data_areas", "20220516-151012-8d0a9a06",
  "ner_data_areas", "20220516-151021-0134f4c1",
  "nga_data_areas", "20220516-151025-71d5143f",
  "rwa_data_areas", "20220516-151038-57dff230",
  "sen_data_areas", "20220519-153956-e993f0fd",
  "sle_data_areas", "20220516-151044-192c4c77",
  "swz_data_areas", "20220516-151121-e82ad159",
  "tcd_data_areas", "20220516-151126-7f44c692",
  "tgo_data_areas", "20220516-151130-d51c5325",
  "tza_data_areas", "20220520-170436-5db07a3e",
  "uga_data_areas", "20220516-151159-b3124896",
  "zmb_data_areas", "20220519-111807-5d8cac85",
  "zwe_data_areas", "20220526-105132-44d5b0fc",
  "mwi_data_areas", "20200929-213435-8f7790a3",
  "zaf_data_areas", "20210419-091902-c45916f6",
)

readr::write_csv(
  ids_used, 
  paste0(save_dir, "ids_used.csv")
)