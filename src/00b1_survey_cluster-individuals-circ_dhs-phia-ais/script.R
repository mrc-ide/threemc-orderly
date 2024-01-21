
# SSA countries
iso3 <- c("ago", "bdi", "ben", "bfa", "bwa", "caf", "civ", "cmr", "cod",
          "cog", "eth", "gab", "gha", "gin", "gmb", "gnb", "gnq", "hti",
          "ken", "lbr", "lso", "mli", "moz", "mwi", "nam", "ner", "nga",
          "rwa", "sen", "sle", "swz", "tcd", "tgo", "tza", "uga", "zmb",
          "zwe")

# ensure save_dir exists
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir)


#' ============================================================
#' !! NOTE 12 November 2023: orderly.sharepoint authentication
#' broken; work around using local file paths

## #### Initial Sharepoint Download ####
## sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

## # function to load data from specific dir on sharepoint
## load_sharepoint_data <- function(
##   path, pattern = NULL, group = "HIVInferenceGroup-WP/"
## ) {
  
##   # List files in folder
##   folder <- sharepoint$folder(group, URLencode(path))
  
##   # pull urls for each file
##   urls <- URLencode(file.path("sites", group, path, folder$files()$name))
  
##   # may only require certain files 
##   if (!is.null(pattern)) {
##     # only want cluster, individuals and circumcision data
##     urls <- urls[grepl(pattern, urls)]
##   }
  
##   # download files, name with urls so we know order of temp files
##   files = lapply(urls, sharepoint$download)
##   if (length(files) == 0) stop("No files found at supplied path")
##   names(files) <- basename(urls)
  
##   return(files)
## }

## shared_path <- "Shared Documents/Circumcision coverage/raw"

load_sharepoint_data <- function(path, pattern = NULL, group = NULL) {

  stopifnot(is.null(group))

  f <- list.files(path, pattern, full.names = TRUE)
  names(f) <- basename(f)
  f <- as.list(f)
  f
}

shared_path <- "~/Imperial College London/HIV Inference Group - WP - Documents/Circumcision coverage/raw/"

#' ============================================================

## zaf data
zaf_path <- file.path(shared_path, "zaf/survey")
zaf_pattern <- paste(c("clusters", "individuals", "circ"), collapse = "|")
zaf_files <- load_sharepoint_data(zaf_path, zaf_pattern)

## mwi data
mwi_path <- file.path(shared_path, "mwi_data_survey_combine")
mwi_pattern <- zaf_pattern
mwi_files <- load_sharepoint_data(mwi_path, mwi_pattern)

## Survey extract (se) data
se_path <- file.path(shared_path, "Survey extract")
se_pattern <- paste(c(
  "survey_individuals_circ_dhs.rds",
  "survey_individuals_ais-reextract.csv",
  "circ_recoded_dhs.rds"
  ), collapse = "|")
se_files <- load_sharepoint_data(se_path, se_pattern)


areas <- sf::read_sf("depends/areas.geojson")

#### Survey Clusters ####

# function to load data, add iso3 column and bind together
load_cluster_data <- function(path) {
  
  # load data
  cluster_list <- lapply(path, read_csv)

  iso3s <- substr(basename(path), 1, 3)
  stopifnot(iso3s %in% iso3)
  
  # add iso3 column and bind together
  names(cluster_list) <- iso3s
  cluster_df <- cluster_list %>% 
  Map(mutate, ., iso3 = toupper(names(.))) %>%
    bind_rows() %>%
    select(iso3, everything())
  
  return(cluster_df)
}

# countries with DHS surveys
iso3_dhs <- iso3[!iso3 %in% c("bwa", "caf", "gnb", "gnq", "hti", "zaf")]

# load dhs survey cluster datasets
dhs_paths <- paste0("depends/", iso3_dhs, "_dhs_clusters.csv")
dhs_clusters <- load_cluster_data(dhs_paths)

phia_survey_id <- c(
  "bwa2021phia",
  "civ2017phia",
  "cmr2017phia",
  "lso2017phia",
  "ken2018phia",
  "mwi2016phia",
  "nam2017phia",
  "rwa2018phia",
  "swz2017phia",
  "tza2016phia",
  "uga2016phia",
  "zmb2016phia",
  "zwe2016phia",
  "zwe2020phia",
  "lso2020phia",
  "swz2021phia"
)

# years of phia surveys for each country


# load phia cluster datasets
phia_paths <- paste0("depends/", phia_survey_id, "_survey_clusters.csv")
phia_clusters <- load_cluster_data(phia_paths)

# bind together both cluster datasets
survey_clusters <- dhs_clusters %>% 
  mutate(dhs_clusters, cluster_id = as.character(cluster_id)) %>% 
  bind_rows(phia_clusters)
  
# add zaf cluster
zaf_clusters <- read_csv(zaf_files$zaf_survey_clusters.csv)
zaf_clusters$iso3 <- "ZAF"
zaf_clusters <- zaf_clusters %>%
  rename(res_type = restype) %>%
  mutate(cluster_id = as.character(cluster_id),
         survey_region_id = as.integer(factor(survey_region_id)))
survey_clusters <- bind_rows(survey_clusters, zaf_clusters)

#### Survey individuals datasets ####

dhs_individuals <- as.list(
  readRDS(se_files$survey_individuals_circ_dhs.rds)
) %>% 
  bind_rows()

# Replace re-extracted datasets to get weights for AIS surveys
dhs_individuals_reextract <- read_csv(
  se_files$`survey_individuals_ais-reextract.csv`
)

dhs_individuals <- dhs_individuals %>%
  filter(!survey_id %in% dhs_individuals_reextract$survey_id) %>%
  bind_rows(
    (dhs_individuals_reextract %>%
      filter(survey_id %in% dhs_individuals$survey_id))
  )

#' Add LSO2014DHS -- custom extract
lso2014dhs_individuals <- read_csv("depends/lso2014dhs_survey_individuals.csv")

stopifnot(!lso2014dhs_individuals$survey_id %in% dhs_individuals$survey_id)

dhs_individuals <- dhs_individuals %>%
  bind_rows(lso2014dhs_individuals)

#' Add recent DHS surveys: BFA2021DHS, CIV2021DHS, GAB2019DHS, KEN2022DHS, TZA2022DHS, GHA2022DHS

dhs_individuals_new <- read_csv("depends/dhs_survey_individuals.csv")
stopifnot(length(intersect(dhs_individuals$survey_id, dhs_individuals_new$survey_id)) == 0)

dhs_individuals <- dhs_individuals %>%
  bind_rows(dhs_individuals_new)


# individual_id has been coded differently than in circumcision dataset

dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}

dhs_individuals <- dhs_individuals %>%
  mutate(
    individual_id = dhs_individual_id(cluster_id, household, line),
    iso3 = substr(survey_id, 1, 3)
  )


phia_paths <- paste0("depends/", phia_survey_id, "_survey_individuals.csv")

phia_individuals <- lapply(phia_paths, read_csv)
names(phia_individuals) <- toupper(phia_survey_id)
phia_individuals <- phia_individuals %>%
  Map(mutate, ., iso3 = toupper(substr(names(.), 1, 3))) %>%
  bind_rows() %>%
  select(survey_id, everything())

survey_individuals <- dhs_individuals %>%
  mutate(cluster_id = as.character(cluster_id),
         household = as.character(household),
         line = as.character(line)) %>%
  bind_rows(phia_individuals)

zaf_individuals <- read_csv(zaf_files$zaf_survey_individuals.csv)
zaf_individuals <- zaf_individuals %>%
  mutate(iso3 = "ZAF",
         household = as.character(household),
         line = as.character(line),
         cluster_id = as.character(cluster_id)) %>%
  rename(ethnicity = pop_group)

survey_individuals <- bind_rows(survey_individuals, zaf_individuals)

#### Survey circumcision ####

dhs_circumcision <- readRDS(se_files$circ_recoded_dhs.rds) %>% 
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

#' Add LSO2014DHS survey (manual extract)

lso2014dhs_circumcision <- read_csv("depends/lso2014dhs_survey_circumcision.csv") %>%
  mutate(
    individual_id = sprintf("%11s", individual_id)
  )

stopifnot(!lso2014dhs_circumcision$survey_id %in% dhs_circumcision$survey_id)

dhs_circumcision <- dhs_circumcision %>%
  bind_rows(lso2014dhs_circumcision)


#' Add recent DHS surveys: BFA2021DHS, CIV2021DHS, GAB2019DHS, KEN2022DHS, TZA2022DHS, GHA2022DHS

dhs_circumcision_new <- read_csv("depends/dhs_survey_circumcision.csv") %>%
  mutate(
    individual_id = sprintf("%11s", individual_id)
  )
stopifnot(length(intersect(dhs_circumcision$survey_id, dhs_circumcision_new$survey_id)) == 0)

dhs_circumcision <- dhs_circumcision %>%
  bind_rows(
    select(dhs_circumcision_new, any_of(names(dhs_circumcision)))
  )


phia_paths <- paste0("depends/", phia_survey_id, "_survey_circumcision.csv")
phia_circumcision <- lapply(phia_paths, read_csv)

names(phia_circumcision) <- toupper(phia_survey_id)

#' To match other PHIA surveys
phia_circumcision$LSO2017PHIA <- phia_circumcision$LSO2017PHIA %>%
  rename(circumcised = circ_status)

phia_circumcision <- phia_circumcision %>% 
  Map(mutate, ., iso3 = substr(names(.), 1, 3)) %>%
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
zaf_circumcision <- read_csv(
  zaf_files$zaf_survey_circumcision.csv
) %>% 
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

mwi_survey_clusters <- read_csv(mwi_files$mwi_survey_clusters.csv)
mwi_survey_individuals <- read_csv(mwi_files$mwi_survey_individuals.csv)
mwi_survey_circumcision <- read_csv(mwi_files$mwi_survey_circumcision.csv)

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

## Restrict to (1) males only and (2) only surveys with circumcision

survey_individuals <- filter(survey_individuals, sex == "male")

survey_clusters <- survey_clusters %>%
  semi_join(distinct(survey_circumcision, iso3, survey_id), by = c("iso3", "survey_id"))
            
survey_individuals <- survey_individuals %>%
  semi_join(distinct(survey_circumcision, iso3, survey_id), by = c("iso3", "survey_id"))
                             

#' ## Validation checks                             

#' All individuals map to a survey cluster
#' * There are a few missing here; for most survey it is a small number and
#'   they correspond to clusters with no location available.
#' * Survey clusters are missing for BEN2006DHS; there are no coordinates for this
#'   survey. Needs to be figured out later
survey_individuals %>%
  anti_join(survey_clusters, by = c("iso3", "survey_id", "cluster_id")) %>%
  count(survey_id)

#' All circumcision in individuals data 
survey_circumcision %>%
  anti_join(survey_individuals, by = c("iso3", "survey_id", "individual_id")) %>%
  count(survey_id) %>%
  nrow() %>%
  {stopifnot(. == 0)}


#' Check age variable complete
#' * 3 missing in ZAF2012HSRC

survey_individuals %>%
  filter(is.na(age)) %>%
  pull(survey_id) %>%
  {stopifnot(. == rep("ZAF2012HSRC", 3))}

stopifnot(survey_circumcision$circ_status %in% c(NA, 0, 1))
stopifnot(survey_circumcision$circ_who %in% c(NA, "medical", "traditional"))
stopifnot(survey_circumcision$circ_where %in% c(NA, "medical", "traditional"))


#### save data #### 

write_csv(survey_clusters, paste0(save_dir, "survey_clusters.csv.gz"))
write_csv(survey_individuals, paste0(save_dir, "survey_individuals.csv.gz"))
write_csv(survey_circumcision, paste0(save_dir, "survey_circumcision.csv.gz"))
