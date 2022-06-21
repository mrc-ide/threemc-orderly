#' Master data set schema:
#' 
#' * iso3         
#' * survey_id    
#' * area_id
#' * area_level
#' * area_name     [Removed because it is added modelling; consider revising this later]
#' * individual_id
#' * cluster_id   
#' * household    
#' * line         
#' * sex          
#' * age          
#' * indweight    
#' * circ_status  
#' * circ_age     
#' * circ_who     
#' * circ_where   



# ensure save_dir exists
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir)

# load areas
areas <- sf::st_drop_geometry(sf::read_sf("depends/areas.geojson"))

#' ## DHS / AIS / PHIA
#'
#' Merge three data sets into single circumcision data set

dhs_clusters <- readr::read_csv("depends/survey_clusters.csv.gz")
dhs_individuals <- readr::read_csv("depends/survey_individuals.csv.gz")
dhs_circumcision <- readr::read_csv("depends/survey_circumcision.csv.gz")

dhs_merged <- dhs_clusters %>%
  select(iso3, survey_id, cluster_id, area_id = geoloc_area_id) %>%
  left_join(
    areas %>%
      select(iso3, area_id, area_level, area_name),
    by = c("iso3", "area_id")
  ) %>%
  inner_join(
    dhs_individuals %>%
      select(iso3, survey_id, individual_id, cluster_id, household, line, sex, age, indweight),
    by = c("iso3", "survey_id", "cluster_id")
  ) %>%
  inner_join(
    dhs_circumcision %>%
      select(iso3, survey_id, individual_id, circ_status, circ_age, circ_who, circ_where),
    by = c("iso3", "survey_id", "individual_id")
  ) %>%
  select(iso3, survey_id, area_id, area_level, area_name,
         individual_id, cluster_id, household, line, sex, age, indweight,
         circ_status, circ_age, circ_who, circ_where)


#' ## MICS surveys

# load previous survey datasets
mics_final <- readr::read_csv("depends/mics_surveys.csv.gz")

mics_final <- mics_final %>%
  mutate(
    cluster_id = as.character(cluster_id),
    household = as.character(household),
    line = as.character(line)
  )

survey_merged <- bind_rows(dhs_merged, mics_final) %>%
  arrange(iso3, survey_id, age, circ_age)

#' Review missing area_id
#' * These should occur due to clusters with no geocoordinates
#' * CHECK: ensure <10% for all surveys [if not, suggests something gone awry]

survey_merged %>%
  group_by(iso3, survey_id) %>%
  summarise(missing_area_id = mean(is.na(area_id)), .groups = "drop") %>%
  pull(missing_area_id) %>%
  {stopifnot(. < 0.1)}

stopifnot(survey_merged$circ_status %in% c(NA, 0, 1))
stopifnot(survey_merged$circ_who %in% c(NA, "traditional", "medical"))
stopifnot(survey_merged$circ_where %in% c(NA, "traditional", "medical"))

survey_merged %>%
  filter(is.na(indweight)) %>%
  count(survey_id) 

#' Check that weight is only missing in three surveys GNB2018MICS, ZAF2002HSRC, ZAF2008HSRC
#' * More added to this list probably means an error has occurred

survey_merged %>%
  filter(is.na(indweight)) %>%
  pull(survey_id) %>%
  {stopifnot(. %in% c("ZAF2002HSRC", "ZAF2008HSRC", "GNB2018MICS"))}

#' Check that all surveys have high completion of circumcision status variable
#' * CHECK: <25% missing values

survey_merged %>%
  group_by(iso3, survey_id) %>%
  summarise(missing_circ_status = mean(is.na(circ_status))) %>%
  arrange(-missing_circ_status)

survey_merged %>%
  group_by(iso3, survey_id) %>%
  summarise(missing_circ_status = mean(is.na(circ_status)), .groups = "drop") %>%
  pull(missing_circ_status) %>%
  {stopifnot(. < 0.3)}


# added in modelling
if ("area_name" %in% names(survey_merged)) {
  survey_merged <- select(survey_merged, -area_name)
}

# save survey_circumcision
readr::write_csv(
  survey_merged,
  file = paste0(save_dir, "survey_circumcision.csv.gz")
)
