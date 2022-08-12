
#' Note: requires branch mrc-ide/naomi.utils@sexbehav-vars-jeff
#'
stopifnot(packageVersion("naomi.utils") >= "0.1.11")

areas <- read_sf("depends/areas.geojson")
areas <- st_drop_geometry(areas)

survey <- read_csv("depends/survey_circumcision.csv.gz")

## survey_meta <- read_csv("depends/zaf_survey_meta.csv")
## survey_regions <- read_csv("depends/zaf_survey_regions.csv")

#' There are some hacks in here because we did not save the survey regions

survey_clusters <- survey %>%
  filter(!is.na(area_id)) %>% 
  select(iso3, survey_id, cluster_id, geoloc_area_id = area_id) %>%
  mutate(survey_region_id = 0L,
         res_type = NA) %>%
  distinct()

survey_individuals <- survey %>%
  select(iso3, survey_id, cluster_id, individual_id, household, line, sex, age, indweight) %>%
  distinct()

survey_circumcision <- survey %>%
  select(iso3, survey_id, individual_id, circ_status, circ_age, circ_who, circ_where)

#' ## Hack creation of survey_regions and survey_meta

survey_meta <- survey_individuals %>%
  group_by(iso3, survey_id, survey_mid_calendar_quarter = substr(survey_id, 4, 7)) %>%
  summarise(
    female_age_min = min(if_else(sex == "female", age, Inf), na.rm = TRUE),
    female_age_max = max(if_else(sex == "female", age, 0), na.rm = TRUE),
    male_age_min = min(if_else(sex == "male", age, Inf), na.rm = TRUE),
    male_age_max = max(if_else(sex == "male", age, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    female_age_max = if_else(female_age_max >= 80, Inf, female_age_max),
    male_age_max = if_else(male_age_max >= 80, Inf, male_age_max)
  )

survey_regions <- survey_clusters %>%
  distinct(iso3, survey_id, survey_region_id, survey_region_area_id = iso3)

survey_circumcision <- survey_circumcision %>%
  rename(circumcised = circ_status) %>%
  group_by(survey_id) %>%
  mutate(
    circ_type = case_when(all(is.na(circ_who)) && all(is.na(circ_where)) ~ NA_character_,
                          circumcised == 0 ~ "none",
                          circ_who == "medical" | circ_where == "medical" ~ "medical",
                          circ_who == "traditional" | circ_where == "traditional" ~ "traditional"),
    circ_medical = as.integer(circ_type == "medical"),
    circ_traditional = as.integer(circ_type == "traditional")
  )

count(survey_circumcision, survey_id, circ_type) %>%
  spread(circ_type, n) %>%
  print(n = Inf)

count(survey_circumcision, survey_id, circ_medical) %>%
  spread(circ_medical, n)

count(survey_circumcision, survey_id, circ_traditional) %>%
  spread(circ_traditional, n)


#' Subset the other survey datasets to only those with circumcision
#' outcomes.

survey_meta <- semi_join(survey_meta, survey_circumcision, by = c("iso3", "survey_id"))
survey_regions <- semi_join(survey_regions, survey_circumcision, by = c("iso3", "survey_id"))
survey_clusters <- semi_join(survey_clusters, survey_circumcision, by = c("iso3", "survey_id"))
survey_individuals <- semi_join(survey_individuals, survey_circumcision, by = c("iso3", "survey_id"))


#' Drop ther iso3 column from circumcision dataset -- to get it through the
#' calc_survey_indicators() function.

survey_circumcision$iso3 <- NULL


#' Overwrite naomi::get_age_groups() with age 15-29 and 30-49 age groups added

get_age_groups_custom <- function() {
    groups <- naomi:::naomi_read_csv(naomi:::system_file("metadata", "meta_age_group.csv"))
    groups$age_group_span <- as.numeric(groups$age_group_span)
    groups <- dplyr::bind_rows(groups,
        tibble::tribble(~age_group, ~age_group_label, ~age_group_start, ~age_group_span, ~age_group_sort_order,
               "Y015_029", "15-29", 15, 15, 32,
               "Y030_049", "30-49", 30, 20, 33,)
      )
    groups
}

environment(get_age_groups_custom) <- asNamespace("naomi")
assignInNamespace("get_age_groups", get_age_groups_custom, ns = "naomi")

if (is.null(mc.cores)) {
    mc.cores <- if(.Platform$OS.type == "windows") 1 else parallel::detectCores()
}

survey_circ_coverage <- calc_survey_indicators(
  survey_meta,
  survey_regions,
  survey_clusters,
  survey_individuals,
  survey_biomarker = NULL,
  survey_other = list(survey_circumcision),
  areas,
  sex = "male",
  area_bottom_level = 5,
  indicators = c("circumcised", "circ_medical", "circ_traditional"),
  stratification = ~survey_id + area_id + sex + age_group
)

circ_coverage <- survey_circ_coverage %>%
  left_join(select(areas, iso3, area_id, area_level), by = "area_id") %>%
  select(iso3, indicator, survey_id, survey_mid_calendar_quarter, area_level, area_id, area_name,
         everything()) %>%
  arrange(iso3, factor(indicator, c("circumcised", "circ_medical", "circ_traditional")), survey_id)

dir.create("artefacts")
write_csv(circ_coverage, "artefacts/survey-circumcision-coverage.csv.gz", na = "")
