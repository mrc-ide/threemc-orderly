# SSA countries
iso3 <- c("ago", "bdi", "ben", "bfa", "bwa", "caf", "civ", "cmr", "cod",
          "cog", "eth", "gab", "gha", "gin", "gmb", "gnb", "gnq", "ken", 
          "lbr", "lso", "mli", "moz", "mwi", "nam", "ner", "nga", "rwa", 
          "sen", "sle", "swz", "tcd", "tgo", "tza", "uga", "zaf", "zmb", "zwe")

# pull through 2021 shapefiles
for (i in iso3) {
  print(toupper(i))
  # failed countries
  if (i %in% c("mwi", "zaf")) {
    spec_id <- "latest" # ensure you still pull in latest task for these iso3s
  } else spec_id <- "latest(parameter:version == 2021)"
  orderly::orderly_pull_archive(
    paste0(i, "_data_areas"),
    remote = "naomi",
    id = spec_id
  )
}
