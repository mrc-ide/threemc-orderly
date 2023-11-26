#' DHS Phase 8 changed circumcision questions
#' Definition of mv438a changes
#'

## mv483       "Respondent circumcised"
## mv483a      "Age at traditional circumcision"
## mv483b      "NA - Who performed the circumcision"
## mv483c      "NA - Place where circumcision was done"
## mv483d      "Is respondent traditionally circumcised"
## mv483e      "Is respondent medically circumcised"
## mv483f      "Age at medical circumcision"

dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}


surveys <- create_surveys_dhs(c("BFA", "CIV", "GAB", "KEN", "TZA"), "DHS", NULL)
surveys <- surveys %>% filter(SurveyYear >= 2019)
dat <- create_individual_hiv_dhs(surveys)
ind <- create_survey_individuals_dhs(dat)

mrd <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "MR", fileFormat = "FL")
has_mv483 <- search_variables(mrd$FileName, "mv483")
mrd <- filter(mrd, SurveyId %in% has_mv483$survey_id) %>%
  left_join(select(surveys, SurveyId, survey_id))
mrd$path <- unlist(get_datasets(mrd))
                   
mr <- lapply(mrd$path, readRDS) %>%
  setNames(surveys$survey_id)

circ_dat <- mr %>%
  lapply(select, mv001, mv002, mv003, mcaseid, starts_with("mv483")) %>%
  Map(mutate, ., survey_id = names(.)) %>%
  bind_rows()


circ_who_recode <- list("medical" = c("health worker/ health professional",
                                                "health worker / professional"),
                        "traditional" = c("family/ friend",
                                          "traditional practitioner/ ngaliba/ anankungwi",
                                          "traditional practitioner / family friend",
                                          "religious leader",
                                          "other"),
                        NULL = c("dk", "don't know", "missing"))

circ_where_recode <- list("medical" = c("health facility",
                                        "home of a health worker / professional",
                                        "home of a health worker/health professional"),
                          "traditional" = c("own home",
                                            "another home",
                                            "other home / place",
                                            "simba",
                                            "ritual site",
                                            "circumcision done at home"),
                          NULL = c("dk", "don't know", "missing"))



circ <- circ_dat %>%
  mutate(
    mv483a = case_when(mv483a < 95 ~ mv483a,
                       mv483a == 95 ~ 0L,
                       mv483a >= 96 ~ NA_integer_),
    mv483f = case_when(mv483f < 95 ~ mv483f,
                       mv483f == 95 ~ 0L,
                       mv483f >= 96 ~ NA_integer_)
  ) %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    survey_id = survey_id,
    individual_id = dhs_individual_id(mv001, mv002, mv003),
    circ_status = recode(zap_labels(mv483), `1` = 1L, `0` = 0L, .default = NA_integer_),
    circ_who = as.character(fct_collapse(as_factor(mv483b), !!!circ_who_recode)),
    circ_where = as.character(forcats::fct_collapse(as_factor(mv483c), !!!circ_where_recode)),
    circ_method = case_when(
      mv483d == 1 & mv483e == 1 ~ "both",
      mv483d == 1 ~ "traditional",
      mv483e == 1 ~ "medical",
      all(is.na(mv483d)) & (circ_where == "medical" | circ_who == "medical") ~ "medical",
      all(is.na(mv483d)) & (circ_where == "traditional" | circ_who == "traditional") ~ "traditional"
    ),
    circ_age = pmin(mv483a, mv483f, na.rm = TRUE),
    circ_med_age = case_when(
      all(is.na(mv483f)) & circ_method %in% c("both", "medical") ~ circ_age,
      circ_method %in% c("both", "medical") ~ mv483f
    ),
    circ_trad_age = case_when(
      all(is.na(mv483f)) & circ_method %in% c("both", "traditional") ~ circ_age,
      circ_method %in% c("both", "traditional") ~ mv483a
    ),
    circ_where = case_when(
      !all(is.na(circ_where)) ~ circ_where,
      circ_method == "both" & circ_med_age <= circ_trad_age ~ "medical",
      circ_method == "both" ~ "traditional",
      circ_method == "medical" ~ "medical",
      circ_method == "traditional" ~ "traditional"
    ),
    circ_where = if_else(circ_where == "NULL", NA_character_, circ_where),
    circ_who = if_else(circ_who == "NULL", NA_character_, circ_who),
    .by = survey_id,
    .keep = "none"
  ) %>%
  select(iso3, survey_id, everything())


dir.create("artefacts")
write_csv(circ, "artefacts/dhs_survey_circumcision.csv", na = "")
write_csv(ind, "artefacts/dhs_survey_individuals.csv", na = "")

