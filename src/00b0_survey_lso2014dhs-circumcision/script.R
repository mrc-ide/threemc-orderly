
mr <- readRDS(get_datasets("LSMR71FL.ZIP")[[1]])

## variable                            description
##    mv483                 Respondent circumcised
##   mv483a                    Age at circumcision
##   mv483b    NA - Who performed the circumcision
##   mv483c NA - Place where circumcision was done
##   sm805a   Respondent traditionally circumcised
##   sm805b        Age at traditional circumcision
##   sm805c       Respondent medically circumcised
##   sm805d            Age at medical circumcision

## Note: label for age <5 years different for mv483a and sm805d:

## > attr(mr$mv483a, "labels")
## during childhood <5 years                don't know                   missing 
##                        95                        98                        99 

## > attr(mr$sm805b, "labels")
## during childhood (< 5 years)                   don't know 
##                           96                           98 

## > attr(mr$sm805d, "labels")
## during childhood (< 5 years)                   don't know 
##                           96                           98 


dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}

#' Notes:
#'
#' * Recode circumcised <5 years as age 0
#' * If circ_med_age <= circ_trad_age, code circ_where = "medical"
#' 

circ <- mr %>%
  mutate(across(c(starts_with("mv483"), starts_with("sm805")), zap_labels)) %>%
  transmute(
    iso3 = "LSO",
    survey_id = "LSO2014DHS",
    individual_id = dhs_individual_id(mv001, mv002, mv003),
    circ_status = recode(mv483, `1` = 1L, `0` = 0L, .default = NA_integer_),
    circ_method = case_when(sm805a == 1 & sm805c == 1 ~ "both",
                            sm805a == 1 ~ "traditional",
                            sm805c == 1 ~ "medical"),
    circ_age = case_when(mv483a < 95 ~ mv483a,
                         mv483a == 95 ~ 0L,
                         mv483a >= 96 ~ NA_integer_),
    circ_trad_age = case_when(sm805b < 96 ~ sm805b,
                              sm805b == 96 ~ 0L,
                              sm805b >= 97 ~ NA_integer_),
    circ_med_age = case_when(sm805d < 96 ~ sm805d,
                             sm805d == 96 ~ 0L,
                             sm805d >= 97 ~ NA_integer_)
  )

    
circ <- circ %>%
  mutate(
    circ_where = case_when(circ_method == "both" & circ_med_age <= circ_trad_age ~ "medical",
                           circ_method == "both" ~ "traditional",
                           TRUE ~ circ_method)
  )

dir.create("artefacts")
write_csv(circ, "artefacts/lso2014dhs_survey_circumcision.csv", na = "")


#' ## Create survey_individuals dataset
surveys <- create_surveys_dhs("LSO", "DHS")
surveys <- filter(surveys, SurveyYear == 2014)
dat <- naomi.utils::create_individual_hiv_dhs(surveys)
ind <- naomi.utils::create_survey_individuals_dhs(dat)

write_csv(ind, "artefacts/lso2014dhs_survey_individuals.csv", na = "")
