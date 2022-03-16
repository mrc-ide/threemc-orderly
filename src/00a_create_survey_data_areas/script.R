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
area_paths <- paste0(iso3, "_areas.geojson")
names(area_paths) <- toupper(iso3)
# replace MWI areas with TA-level for linking survey data
area_paths["MWI"] = "mwi_areas_ta.geojson"

# load shapefiles
areas <- lapply(area_paths, threemc::read_circ_data)

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
# areas_test <- sf::read_sf("~/OneDrive/data/areas_orig.geojson")
# anti_join(sf::st_drop_geometry(areas), sf::st_drop_geometry(areas_test)) %>%
#   distinct(iso3)

#### Survey Clusters ####

# function to load data, add iso3 column and bind together
load_cluster_data <- function(path) {
  
  # load data
  cluster_list <- lapply(path, threemc::read_circ_data)
  # add iso3 column and bind together
  names(cluster_list) <- substr(path, 0, 3)
  cluster_df <- cluster_list %>% 
  Map(mutate, ., iso3 = toupper(names(.))) %>%
    bind_rows() %>%
    select(iso3, everything())
  
  return(cluster_df)
}

# countries with DHS surveys
iso3_dhs <- iso3[!iso3 %in% c("bwa", "caf", "gnb", "gnq", "hti", "zaf")]

# load dhs survey cluster datasets
dhs_paths <- paste0(iso3_dhs, "_dhs_clusters.csv")
dhs_clusters <- load_cluster_data(dhs_paths)

# countries with phia surveys
iso3_phia <- c(
  "civ", "cmr", "lso", "mwi", "nam", "rwa",
  "swz", "tza", "uga", "zmb", "zwe"
)
# years of phia surveys for each country
phia_years <- c(
  2017, 2017, 2017, 2016, 2017, 2018, 2017, 2016, 2016, 2016, 2016
)

# load phia cluster datasets
phia_paths <- paste0(iso3_phia, phia_years, "phia_survey_clusters.csv")
phia_clusters <- load_cluster_data(phia_paths)

# bind together both cluster datasets
survey_clusters <- dhs_clusters %>% 
  mutate(dhs_clusters, cluster_id = as.character(cluster_id)) %>% 
  bind_rows(phia_clusters)
  
# add zaf cluster
zaf_clusters <- threemc::read_circ_data("zaf_survey_clusters.csv")
zaf_clusters$iso3 <- "ZAF"
zaf_clusters <- zaf_clusters %>%
  rename(res_type = restype) %>%
  mutate(cluster_id = as.character(cluster_id),
         survey_region_id = as.integer(factor(survey_region_id)))
survey_clusters <- bind_rows(survey_clusters, zaf_clusters)

#### Survey individuals datasets ####

dhs_individuals <- as.list(readRDS("survey_individuals_circ_dhs.rds")) %>% 
  bind_rows() %>%
  mutate(iso3 = substr(survey_id, 1, 3))

# Replace re-extracted datasets to get weights for AIS surveys
dhs_individuals_reextract <- threemc::read_circ_data(
  "survey_individuals_ais-reextract.csv"
)

dhs_individuals <- dhs_individuals %>%
  filter(!survey_id %in% dhs_individuals_reextract$survey_id) %>%
  bind_rows(
    (dhs_individuals_reextract %>%
      filter(survey_id %in% dhs_individuals$survey_id))
  )

# individual_id has been coded differently than in circumcision dataset

dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}

dhs_individuals <- dhs_individuals %>%
  mutate(individual_id = dhs_individual_id(cluster_id, household, line))

# phia_paths <- file.path(here::here("raw/fertility_orderly/archive"),
#                         paste0(iso3_phia, "_survey_phia")) %>%
#   setNames(iso3_phia)
# phia_paths <- list.files(phia_paths, "individuals.csv",
#                          recursive = TRUE, full.names = TRUE) %>%
#   setNames(iso3_phia)

phia_paths <- paste0(iso3_phia, phia_years, "phia_survey_individuals.csv")

phia_individuals <- lapply(phia_paths, threemc::read_circ_data)
names(phia_individuals) <- toupper(iso3_phia)
phia_individuals <- phia_individuals %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  select(iso3, everything())

survey_individuals <- dhs_individuals %>%
  mutate(cluster_id = as.character(cluster_id),
         household = as.character(household),
         line = as.character(line)) %>%
  bind_rows(phia_individuals)

zaf_individuals <- read_csv("zaf_survey_individuals.csv")
zaf_individuals <- zaf_individuals %>%
  mutate(iso3 = "ZAF",
         household = as.character(household),
         line = as.character(line),
         cluster_id = as.character(cluster_id)) %>%
  rename(ethnicity = pop_group)

survey_individuals <- bind_rows(survey_individuals, zaf_individuals)

#### Survey circumcision ####

dhs_circumcision <- readRDS("circ_recoded_dhs.rds") %>% 
  bind_rows() %>% 
  mutate(individual_id = as.character(individual_id))

dhs_circumcision <- as.list(dhs_circumcision) %>%
  bind_rows() %>%
  mutate(iso3 = substr(survey_id, 1, 3)) %>% 
  # Only use DHS for now; take PHIA from the other extracts; MICS later
  filter(
    sub("^[A-Z]{3}[0-9]{4}", "", survey_id) %in% c("AIS", "DHS"),
    # Drop CIV2005AIS for now: individual_id and houshold not coded consistently
    survey_id != "CIV2005AIS"
  )

phia_paths <- paste0(iso3_phia, phia_years, "phia_survey_circumcision.csv")
phia_circumcision <- lapply(phia_paths, threemc::read_circ_data)
names(phia_circumcision) <- toupper(iso3_phia)
phia_circumcision <- phia_circumcision %>% 
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  select(iso3, everything()) %>%
  rename(circ_status = circumcised) %>%
  mutate(
    circ_who = circ_who %>%
      recode("Traditional practitioner" = "traditional",
             "Healthcare worker" = "medical")
  )

survey_circumcision <- dhs_circumcision %>%
  mutate(cluster_id = as.character(cluster_id),
         household = as.character(household),
         line = as.character(line)) %>%
  bind_rows(phia_circumcision) %>% 
  # Drop cluster_id, household, line columns; redundant with individual_id
  select(iso3, everything(), -c(cluster_id, household, line)) %>% 
  # Recode "other" circ_who to "traditional"
  mutate(circ_who = recode(circ_who, "other" = "traditional"))

# load ZAF data
zaf_circumcision <- read_csv("zaf_survey_circumcision.csv")
zaf_circumcision <- zaf_circumcision %>%
  rename(circ_status = circ) %>%
  mutate(
    iso3 = "ZAF",
    circ_who = circ_who %>%
      recode("Healthcare worker" = "medical",
             "Traditional practitioner" = "traditional"),
    circ_where = tolower(circ_where)
  )

survey_circumcision <- bind_rows(survey_circumcision, zaf_circumcision)

#### Replace Malawi data with mwi-hiv-orderly version (more surveys) #### 

mwi_survey_clusters <- read_circ_data("mwi_survey_clusters.csv")
mwi_survey_individuals <- read_circ_data("mwi_survey_individuals.csv")
mwi_survey_circumcision <- read_circ_data("mwi_survey_circumcision.csv")

survey_clusters <- survey_clusters %>%
  filter(iso3 != "MWI") %>%
  bind_rows(mutate(mwi_survey_clusters, iso3 = "MWI"))

survey_individuals <- survey_individuals %>%
  filter(iso3 != "MWI") %>%
  bind_rows(mutate(mwi_survey_individuals, iso3 = "MWI")) %>%
  select(iso3, everything())

survey_circumcision <- survey_circumcision %>%
  filter(iso3 != "MWI") %>%
  bind_rows(
    mwi_survey_circumcision %>%
      rename(circ_status = circumcised) %>%
      mutate(
        iso3 = "MWI",
        circ_where = tolower(circ_where),
        circ_who = recode(
          circ_who, 
          "Healthcare worker" = "medical", 
          "Traditional practitioner" = "traditional"
        )
      )
  ) %>%
  select(iso3, everything())

#### save data #### 

sf::write_sf(areas, "areas.geojson")
write_csv(survey_clusters, "survey_clusters.csv.gz")
write_csv(survey_individuals, "survey_individuals.csv.gz")
write_csv(survey_circumcision, "survey_circumcision.csv.gz")