library(dplyr)
library(readr)
library(sf)

# recall: read data from fertility_orderly, write to survey_circumcision!
# copy over orderly stuff from fertility_orderly to this repo

#### Area hierarchy ####
#'
#' Sourced from osymandius/fertility_orderly orderly archive.
#'

setwd("..") # to be run from top level of circumcision_coverage

iso3 <- c("ago", "bdi", "ben", "bfa", "bwa", "caf", "civ", "cmr", "cod",
          "cog", "eth", "gab", "gha", "gin", "gmb", "gnb", "gnq", "hti",
          "ken", "lbr", "lso", "mli", "moz", "mwi", "nam", "ner", "nga",
          "rwa", "sen", "sle", "swz", "tcd", "tgo", "tza", "uga", "zmb",
          "zwe")

## TO DO: add South Africa

## lapply(paste0(iso3, "_data_areas"), orderly::orderly_pull_archive)

#' Note: the logic in this code relies on there only being a single file stored
#' in the archive/ folder for each <iso3>_data_areas task.
#'
#' This is very fragile until properly in an orderly workflow! Be cautious
#' when running / just make Jeff update it.

area_paths <- file.path(here("raw/fertility_orderly/archive"),
                        paste0(iso3, "_data_areas")) %>%
  setNames(iso3)

#' Should all be length = 1.  If >1, there is more than one <iso3>_data_areas
#' tasks in archive and one needs to be deleted.
n_area_tasks <- lengths(lapply(area_paths, list.files))
n_area_tasks[which(n_area_tasks > 1)]

area_paths <- list.files(area_paths, "areas.geojson",
                         recursive = TRUE,
                         full.names = TRUE) %>%
  setNames(iso3)

areas <- lapply(area_paths, read_sf)

#' Clean GMB area hierarchy geometry. (This should be done way upstring)
areas[["gmb"]] <- st_collection_extract(areas[["gmb"]], "POLYGON")

areas <- areas %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  lapply(select, -any_of("epp_level")) %>%
  bind_rows() %>%
  select(iso3, everything())

areas <- areas %>%
  select(-spectrum_level, -naomi_level, -pepfar_psnu_level)

#' Add South Africa areas
zaf_areas <- read_sf(here("raw/zaf/zaf_areas.geojson"))
zaf_areas <- zaf_areas %>%
  filter(area_level <= 2) %>%
  mutate(iso3 = "ZAF") %>%
  select(all_of(names(areas)))

areas <- areas %>%
  bind_rows(zaf_areas)

write_sf(areas, here("data", "areas.geojson"))


#### Survey clusters datasets ####

#' * MICS surveys currently cannot be used in modelling for most countries
#'   because cluster coordinates are not available.
#'

iso3_dhs <- iso3[!iso3 %in% c("bwa", "gnb", "gnq", "hti", "caf")]
lapply(paste0(iso3_dhs, "_survey"), orderly::orderly_pull_archive,
       remote = "real")

# setwd("../circumcision-coverage/") # revert directory

dhs_paths <- file.path(here("raw/fertility_orderly/archive"),
                       paste0(iso3_dhs, "_survey")) %>%
  setNames(iso3_dhs)

dhs_paths <- list.files(dhs_paths, "dhs_clusters.csv",
                        recursive = TRUE, full.names = TRUE) %>%
  setNames(substr(basename(.), 1, 3))

dhs_clusters <- lapply(dhs_paths, read_csv)

dhs_clusters <- dhs_clusters %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  select(iso3, everything())

iso3_phia <- c("civ", "cmr", "lso", "mwi", "nam", "rwa", "swz", "tza", "uga", "zmb", "zwe")
lapply(paste0(iso3_phia, "_survey_phia"),
       orderly::orderly_pull_archive,
       remote = "real")

# phia_paths <- file.path(here::here("raw/fertility_orderly/archive"),
phia_paths <- file.path(paste0(getwd(), "/archive"),
                        paste0(iso3_phia, "_survey_phia")) %>%
  setNames(iso3_phia)
phia_paths <- list.files(phia_paths, "clusters.csv",
                         recursive = TRUE, full.names = TRUE) %>%
  setNames(iso3_phia)

phia_clusters <- lapply(phia_paths, read_csv)

phia_clusters <- phia_clusters %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  select(iso3, everything())

survey_clusters <- bind_rows(
  mutate(dhs_clusters, cluster_id = as.character(cluster_id)),
  phia_clusters
)

zaf_clusters <- read_csv(here("raw/zaf/survey/zaf_survey_clusters.csv"))
zaf_clusters$iso3 <- "ZAF"
zaf_clusters <- zaf_clusters %>%
  rename(res_type = restype) %>%
  mutate(cluster_id = as.character(cluster_id),
         survey_region_id = as.integer(factor(survey_region_id)))

survey_clusters <- bind_rows(survey_clusters, zaf_clusters)

write_csv(survey_clusters, here("data", "survey_clusters.csv"))

#### Survey individuals datasets ####

dhs_individuals <- readRDS(here::here("raw/Survey extract/survey_individuals_circ.rds"))

dhs_individuals <- dhs_individuals %>%
  as.list() %>%
  bind_rows() %>%
  mutate(iso3 = substr(survey_id, 1, 3))

#' individual_id has been coded differently than in circumcision dataset

dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}

dhs_individuals <- dhs_individuals %>%
  mutate(individual_id = dhs_individual_id(cluster_id, household, line))

# phia_paths <- file.path(here::here("raw/fertility_orderly/archive"),
phia_paths <- file.path(
  # "../circumcision-coverage/raw/fertility_orderly/archive",
  paste0(getwd(), "/archive"),
  paste0(iso3_phia, "_survey_phia")
) %>%
  setNames(iso3_phia)
phia_paths <- list.files(phia_paths, "individuals.csv",
                         recursive = TRUE, full.names = TRUE) %>%
  setNames(iso3_phia)

phia_individuals <- lapply(phia_paths, read_csv)

phia_individuals <- phia_individuals %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  select(iso3, everything())

survey_individuals <- dhs_individuals %>%
  mutate(cluster_id = as.character(cluster_id),
         household = as.character(household),
         line = as.character(line)) %>%
  bind_rows(phia_individuals)

zaf_individuals <- read_csv(here("raw/zaf/survey/zaf_survey_individuals.csv"))
zaf_individuals <- zaf_individuals %>%
  mutate(iso3 = "ZAF",
         household = as.character(household),
         line = as.character(line),
         cluster_id = as.character(cluster_id)) %>%
  rename(ethnicity = pop_group)

survey_individuals <- bind_rows(survey_individuals, zaf_individuals)

write_csv(survey_individuals, here("data", "survey_individuals.csv.gz"))


#### Survey circumcision ####

old <- readRDS(here::here("raw/Survey extract/circ_recoded.rds"))
dhs_circumcision <- readRDS(here::here("raw/Survey extract/circ_recoded_dhs.rds"))
dhs_circumcision <- bind_rows(dhs_circumcision)

dhs_circumcision <- dhs_circumcision %>%
  mutate(individual_id = as.character(individual_id))

dhs_circumcision <- dhs_circumcision %>%
  as.list() %>%
  bind_rows() %>%
  mutate(iso3 = substr(survey_id, 1, 3))

#' Only use DHS for now; take PHIA from the other extracts; MICS later
#'

dhs_circumcision %>%
  count(survey_type = sub("^[A-Z]{3}[0-9]{4}", "", survey_id))

dhs_circumcision <- dhs_circumcision %>%
  filter(sub("^[A-Z]{3}[0-9]{4}", "", survey_id) %in% c("AIS", "DHS"))

#' Drop CIV2005AIS for now: individual_id and houshold not coded consistently

dhs_circumcision <- filter(dhs_circumcision, survey_id != "CIV2005AIS")

phia_paths <- file.path(here::here("raw/fertility_orderly/archive"),
                        paste0(iso3_phia, "_survey_phia")
) %>%
  setNames(iso3_phia)
phia_paths <- list.files(phia_paths, "circumcision.csv",
                         recursive = TRUE, full.names = TRUE) %>%
  setNames(iso3_phia)

phia_circumcision <- lapply(phia_paths, read_csv)

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
  bind_rows(phia_circumcision)

#' Drop cluster_id, household, line columns; redundant with individual_id
survey_circumcision <- survey_circumcision %>%
  select(-cluster_id, -household, -line) %>%
  select(iso3, everything())

#' Recode "other" circ_who to "traditional"

survey_circumcision <- survey_circumcision %>%
  mutate(circ_who = recode(circ_who, "other" = "traditional"))


zaf_circumcision <- read_csv(here("raw/zaf/survey/zaf_survey_circumcision.csv"))
zaf_circumcision <- zaf_circumcision %>%
  rename(circ_status = circ) %>%
  mutate(iso3 = "ZAF",
         circ_who = circ_who %>%
           recode("Healthcare worker" = "medical",
                  "Traditional practitioner" = "traditional"),
         circ_where = tolower(circ_where))

survey_circumcision <- bind_rows(survey_circumcision, zaf_circumcision)

write_csv(survey_circumcision, here("data", "survey_circumcision.csv.gz"))


#' ## Check foreign key constraints

survey_individuals %>%
  anti_join(survey_clusters, by = c("survey_id", "cluster_id")) %>%
  count(survey_id)

survey_circumcision %>%
  anti_join(survey_individuals, by = c("survey_id", "individual_id")) %>%
  count(survey_id)