
iso3 <- "MOZ"
country <- "Mozambique"
survey_id  <- "MOZ2021PHIA"
survey_mid_calendar_quarter <- "CY2021Q3"


#' ## Load area hierarchy
areas <- read_sf("depends/moz_areas.geojson")


## #' ## Load PHIA datasets

phia_path <- "~/Data/household surveys/PHIA/datasets/MOZ2021PHIA/datasets/"

phia_files <- list(geo = "INSIDA 2021 Geospatial Data (DTA).zip",
                   data = "INSIDA 2021 Household Interview and Biomarker Datasets (DTA).zip") %>%
  lapply(\(x) file.path(phia_path, x))


geo <- rdhs::read_zipdata(phia_files$geo)

hh <- rdhs::read_zipdata(phia_files$data, "insida2021hh.dta")
bio <- rdhs::read_zipdata(phia_files$data, "insida2021adultbio.dta")
ind <- rdhs::read_zipdata(phia_files$data, "insida2021adultind.dta")
roster <- rdhs::read_zipdata(phia_files$data, "insida2021roster.dta")

#' Note: Religion and ethnic group were not asked. CD4 count as not done
#' 
phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, province, urban, householdid,
         personid, surveystyear, surveystmonth,
         intwt0, gender, age,
         mcstatus, mcage, mcwho_mz, mcloc_mz, mcdetail) %>%
  full_join(
    bio %>%
    filter(bt_status == 1) %>%
    select(personid, btwt0, hivstatusfinal, arvstatus, artselfreported,
           vls, recentlagvlarv),
    by = "personid"
  ) %>%
  mutate(survey_id = survey_id,
         cluster_id = centroidid)


#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty.

hh$survey_region_id <- hh$province # different in each survey dataset depending on survey stratification
phia$survey_region_id <- phia$province # different in each survey dataset depending on survey stratification

survey_region_id <- c("Niassa"            = 1 ,
                      "Cabo Delgado"      = 2 ,
                      "Nampula"           = 3 ,
                      "Zambezia"          = 4 ,
                      "Tete"              = 5 ,
                      "Manica"            = 6 ,
                      "Sofala"            = 7 ,
                      "Inhumbane"         = 8 ,
                      "Gaza"              = 9 ,
                      "Maputo Provincia"  = 10,
                      "Maputo Cidade"     = 11)

areas %>%
  filter(area_level == 1) %>%
  st_drop_geometry() %>%
  select(area_id, area_name) %>%
  arrange(area_id)

survey_region_area_id <- c("Niassa"            = "MOZ_1_11",
                           "Cabo Delgado"      = "MOZ_1_10",
                           "Nampula"           = "MOZ_1_09",
                           "Zambezia"          = "MOZ_1_08",
                           "Tete"              = "MOZ_1_07",
                           "Manica"            = "MOZ_1_06",
                           "Sofala"            = "MOZ_1_05",
                           "Inhumbane"         = "MOZ_1_04",
                           "Gaza"              = "MOZ_1_03",
                           "Maputo Provincia"  = "MOZ_1_12",
                           "Maputo Cidade"     = "MOZ_1_12")

survey_regions <- tibble(survey_id = survey_id,
                         survey_region_id = survey_region_id,
                         survey_region_name = names(survey_region_id),
                         survey_region_area_id = survey_region_area_id[names(survey_region_id)])


#' Add survey region boundary

survey_regions <- survey_regions %>%
  left_join(
    areas %>% select(survey_region_area_id = area_id)
  ) %>%
  st_as_sf()

p_survey_regions <- ggplot(survey_regions) +
  geom_sf(aes(fill = survey_region_name), color = "grey60", alpha = 0.6) +
  geom_sf(data = areas %>% filter(area_level == 1), fill = NA, inherit.aes = FALSE)

dir.create("check")
ggsave("check/moz2021phia-survey-region-boundaries.png", p_survey_regions, h = 7, w = 7)

#' Inspect area_id assigments to confirm

survey_regions %>%
  left_join(
    areas %>%
    as.data.frame() %>%
    select(area_id, area_name, area_level, area_level_label),
    by = c("survey_region_area_id" = "area_id")
  )


#' *** Should not require edits beyond this point ***

#' ## Survey clusters dataset
#'
#' This data frame maps survey clusters to the highest level in the area hiearchy
#' based on geomasked cluster centroids, additionally checking that the geolocated
#' areas are contained in the survey region.

survey_clusters <- hh %>%
  transmute(survey_id = survey_id,
            cluster_id = centroidid,
            cluster_id,
            res_type = factor(urban, 1:2, c("urban", "rural")),
            survey_region_id) %>%
  distinct() %>%
  left_join(geo, by = c("cluster_id" = "CentroidID")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE) %>%
  sf::`st_crs<-`(4326)


#' Snap clusters to areas
#' 
#' This is slow because it maps to the lowest level immediately
#' It would be more efficient to do this recursively through
#' the location hierarchy tree -- but not worth the effort right now.

#' Create a list of all of the areas within each survey region
#' (These are the candidate areas where a cluster could be located)

survey_region_areas  <- survey_regions %>%
  st_join(
    st_point_on_surface(areas) %>%
    filter(area_level == max(area_level)) %>%
    select(area_id)
  ) %>%
  st_set_geometry(NULL) %>%
  left_join(
    areas %>% select(area_id, geometry),
    by = "area_id"
  ) %>%
  select(survey_region_id, area_id, geometry_area = geometry)

#' Calculate distance to each candidate area for each cluster

survey_clusters <- survey_clusters %>%
  left_join(survey_region_areas, by = "survey_region_id",
            relationship = "many-to-many") %>%
  mutate(
    distance = unlist(Map(sf::st_distance, geometry, geometry_area))
  )

#' Keep the area with the smallest distance from cluster centroid.
#' (Should be 0 for almost all)

survey_clusters <- survey_clusters %>%
  arrange(distance) %>%
  group_by(survey_id, cluster_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame() %>%
  transmute(survey_id,
            cluster_id,
            res_type,
            survey_region_id,
            longitude,
            latitude,
            geoloc_area_id = area_id,
            geoloc_distance = distance)

#' Review clusters outside admin area

survey_clusters %>%
  filter(geoloc_distance > 0) %>%
  arrange(-geoloc_distance) %>%
  left_join(survey_regions) 



#' ## Survey individuals dataset

mcstatus_labels <- c(`1` = "Yes, fully circumcised",
                     `2` = "Yes, partially circumcised",
                     `3` = "No",
                     `-8` = "Don't know",
                     `-9` = "Refused")

mcstatus_recode <- c(`1` = "Yes",
                     `2` = "Yes",
                     `3` = "No",
                     `-8` = NA_character_,
                     `-9` = NA_character_)

mcwho_labels = c(`1` = "Traditional Practitioner/ Family/ Friend",
                 `2` = "Medical Provider",
                 `3` = "Both",
                 `-8` = "Don't know",
                 `-9` = "Refused")

mcwho_recode = c(`1` = "Traditional practitioner",
                 `2` = "Healthcare worker",
                 `3` = "Healthcare worker", 
                 `96` = "Traditional practitioner",
                 `-8` = NA_character_,
                 `-9` = NA_character_)

mcloc_labels = c(`1` = "Health Facility",
                 `2` = "Health Provider's Home",
                 `3` = "Own Home",
                 `4` = "During Initiation Rites",
                 `5` = "Mobile Clinic",
                 `96` = "Other",
                 `-8` = "Don't know",
                 `-9` = "Refused")

mcloc_recode = c(`1` = "medical",
                 `2` = "medical",
                 `3` = "traditional",
                 `4` = "traditional",
                 `5` = "medical",
                 `96` = "traditional",
                 `-8` = NA_character_,
                 `-9` = NA_character_)

mcdetail_who_recode <- c(`1` = "Healthcare worker",
                         `2` = "Traditional practitioner")


#' Create individuals data

survey_individuals <-
  phia %>%
  transmute(
    survey_id,
    cluster_id,
    individual_id = personid,
    household = householdid,
    line = personid,
    interview_cmc = 12 * (surveystyear - 1900) + surveystmonth,
    sex = factor(gender, 1:2, c("male", "female")),
    age,
    dob_cmc = NA,
    indweight = intwt0
  ) %>%
  mutate(age = as.integer(age),
         indweight = indweight / mean(indweight, na.rm=TRUE)) 


survey_biomarker <-
  phia %>%
    filter(!is.na(hivstatusfinal)) %>%
    transmute(
      survey_id,
      individual_id = personid,
      hivweight = btwt0,
      hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                            hivstatusfinal == 2 ~ 0),
      arv = case_when(arvstatus == 1 ~ 1,
                      arvstatus == 2 ~ 0),
      artself = case_when(artselfreported == 1 ~ 1,
                          artselfreported == 2 ~ 0),
      vls = case_when(vls == 1 ~ 1,
                      vls == 2 ~ 0),
      recent = case_when(recentlagvlarv == 1 ~ 1,
                         recentlagvlarv == 2 ~ 0)
  ) %>%
  mutate(hivweight = hivweight / mean(hivweight, na.rm = TRUE))


survey_circumcision <- phia %>%
  filter(gender == 1) %>%
  transmute(
    survey_id,
    individual_id = personid,
    circumcised = recode(as.integer(mcstatus),
                         `3` = 0L , `1` = 1L, `2` = 1L, .default = NA_integer_),
    circ_age = mcage,
    circ_where = recode(as.integer(mcloc_mz), !!!mcloc_recode, .default = NA_character_),
    circ_who = recode(as.integer(mcdetail), !!!mcdetail_who_recode, .default = NA_character_)
  )



survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            .groups = "drop") %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         country = country,
         survey_type = "PHIA",
         survey_mid_calendar_quarter = survey_mid_calendar_quarter,
         fieldwork_start = NA,
         fieldwork_end   = NA)

#' ## Save survey datasets

dir.create("artefacts")
write_csv(survey_meta, paste0("artefacts/", tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0("artefacts/", tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0("artefacts/", tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0("artefacts/", tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_biomarker, paste0("artefacts/", tolower(survey_id), "_survey_biomarker.csv"), na = "")
write_csv(survey_circumcision, paste0("artefacts/", tolower(survey_id), "_survey_circumcision.csv"), na = "")
