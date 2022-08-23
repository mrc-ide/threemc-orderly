#### Part 1 of Analysis (Modelling) with functionalised behaviour ####

#################
#### Initial ####
#################

### Metadata to run the models

# !! Change this to use dataset stored in threemc
k_dt <- 5 # Age knot spacing
start_year <-  2006
cens_age <- 59
N <- 1000

# Model BWA
cntry <- "BWA"

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

# remove circumcisions with missing type?
rm_missing_type <- FALSE

# read in data, filter for specific country and male surveys only
areas_orig <- areas <- read_circ_data("depends/areas.geojson") %>% 
  filter(iso3 %in% c(cntry, add_cntry)) %>% 
  # dplyr::mutate(space = 1:dplyr::n()) # add space column to areas
  # "trick" model into thinking BWA and ZAF are one country
  mutate(
    area_id = ifelse(
      grepl(add_cntry, area_id),
      paste0(area_id, paste0("_", tolower(add_cntry))),
      area_id
    ),
    area_id = stringr::str_replace_all(area_id, add_cntry, "BWA"),
    parent_area_id = ifelse(
      grepl(add_cntry, parent_area_id),
      paste0(parent_area_id, paste0("_", tolower(add_cntry))),
      parent_area_id
    ),
    parent_area_id = stringr::str_replace_all(parent_area_id, add_cntry, "BWA"),
    iso3 = "BWA"
  ) %>% 
  filter(area_id != paste0("BWA_", tolower(add_cntry))) %>% 
  arrange(area_id, area_level) %>% 
  mutate(space = 1:dplyr::n()) # add space column to areas

areas <- st_make_valid(areas)
survey_circumcision <- read_circ_data("depends/survey_circumcision.csv.gz") %>% 
  filter(iso3 %in% c(cntry, add_cntry))
survey_circumcision <- survey_circumcision %>% 
  mutate(
    area_id = ifelse(
      grepl(add_cntry, area_id),
      paste0(area_id, paste0("_", tolower(add_cntry))),
      area_id
    ),
    area_id = stringr::str_replace_all(area_id, add_cntry, "BWA"),
    iso3 = "BWA",
    survey_id = ifelse(
      grepl(add_cntry, survey_id),
      paste0(survey_id, paste0("_", tolower(add_cntry))),
      survey_id
    ),
    survey_id = stringr::str_replace_all(survey_id, add_cntry, "BWA"),
  ) %>% 
  arrange(area_id, area_level)

populations <- read_circ_data("depends/population_singleage_aggr.csv.gz") %>% 
  filter(iso3 %in% c(cntry, add_cntry))
populations <- populations %>% 
  mutate(
    area_id = ifelse(
      grepl(add_cntry, area_id),
      paste0(area_id, paste0("_", tolower(add_cntry))),
      area_id
    ),
    area_id = stringr::str_replace_all(area_id, add_cntry, "BWA"),
    iso3 = "BWA"
  ) %>% 
  arrange(area_id, area_level)

# pull recommended area hierarchy for target country
# area_lev <- threemc::datapack_psnu_area_level %>%
#   filter(iso3 == cntry) %>%
#   pull(psnu_area_level)
# 
# # don't model at the country level
# if (length(area_lev) > 0 && area_lev == 0) area_lev <- NULL 
# 
# # if area_level is missing, assume most common area lev in surveys
# if (length(area_lev) == 0) {
#   survey_circ_area_lev <- filter(survey_circumcision, iso3 == cntry)
#   area_lev <- table(as.numeric(substr(survey_circ_area_lev$area_id, 5, 5)))
#   area_lev <- as.numeric(names(area_lev)[area_lev == max(area_lev)])
# }

area_lev <- 1

#### Preparing circumcision data ####
# pull latest censoring year from survey_id
cens_year <- max(as.numeric(
  substr(unique(survey_circumcision$survey_id), 4, 7)
))

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circumcision <- prepare_survey_data(
  areas               = areas,
  # remove area_level column to avoid duplicating columns in prepare_survey_data
  survey_circumcision = select(survey_circumcision, -matches("area_level")),
  area_lev            = area_lev,
  start_year          = start_year,
  cens_year           = cens_year,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
) # %>% 
  # bind_rows()

if (nrow(survey_circumcision) == 0) {
  message("no valid surveys at this level") # move inside function!
}

# include indicator to determine whether there is any type distinction for cntry
if (all(is.na(survey_circumcision$circ_who) &
        is.na(survey_circumcision$circ_where))) {
  print("No type distinction made in valid surveys for this country")
  is_type <- FALSE
} else is_type <- TRUE


#### Shell dataset to estimate empirical rate ####

# Skeleton dataset

# Shell dataset creation changed in the following ways:
#    1) Single age population counts are now added to the output data set.
#       This is needed early on, as we will be aggregating the estimated
#       probabilities and cumulative incidence from the model on the district
#       level in order to include survey data not on the district level (or
#       administrative level of interest). These will be weighted by population.
#    2) Now produced for multiple levels of area_level. Function sets
#       up the shell dataset for all admin boundaries between national (admin 0)
#       and the district level (or administrative level of interest) rather
#       than letting survey_circumcision dictate one level of interest
#
# The internal aggregating to get the obs_mmc etc. still works as the functions
# now uses the new "space" variable defined above. These functions treat each
# "space" as a stratification variable and therefore self-contained. This has
# implications later where we have to specify the administrative boundaries
# we are primarily modelling on.

# TODO: Some countries only have the single age population on the aggregation
# of interest so will need to be changed as we roll this update out.
out <- create_shell_dataset(
  survey_circumcision = survey_circumcision,
  populations     = populations,
  areas               = areas,
  area_lev            = area_lev,
  time1               = "time1",
  time2               = "time2",
  strat               = "space",
  age                 = "age",
  circ                = "indweight_st"
)

#### Dataset for modelling ####

# # temporary fix required due to missing populations for ETH and MOZ
# if (cntry %in% c("ETH", "MOZ")) {
#     missing_pop_areas <- out %>%
#         filter(is.na(population)) %>%
#         distinct(area_id) %>%
#         pull()
#     if (length(missing_pop_areas) > 0) {
#         message(paste0(
#             paste(missing_pop_areas, collapse = ", "),
#             " are missing populations, and will be removed"
#         ))
#         # remove areas with missing populations, change "space" accordingly
#         areas <- areas %>%
#             filter(!area_id %in% missing_pop_areas) %>%
#             mutate(space = 1:n())
#         out <- out %>%
#             filter(!is.na(population)) %>%
#             left_join(areas %>%
#                           select(area_id, space_new = space),
#                       by = "area_id") %>%
#             mutate(space = space_new) %>%
#             select(-space_new)
#     }
# }

# TEST
# out_test <- out %>% 
#   mutate(
#     area_id = ifelse(
#       grepl(add_cntry, area_id),
#       paste0(area_id, paste0("_", tolower(add_cntry))),
#       area_id
#     ),
#     area_id = stringr::str_replace_all(area_id, add_cntry, "BWA")
#   ) %>% 
#   arrange(area_id, area_level)

dat_tmb <- threemc_prepare_model_data(
  out        = out,
  areas      = areas,
  area_lev   = area_lev,
  aggregated = TRUE,
  weight     = "population",
  k_dt       = k_dt
)


#### Modelling circumcision probabilites ####
# specify TMB model
# if (is_type == TRUE) {
#   mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
# } else mod <- "Surv_SpaceAgeTime"
mod <- "Surv_SpaceAgeTime_ByType_withUnknownType_No_AgeTime_Interaction"

# Initial values
parameters <- with(
  dat_tmb,
  list(
    # intercept
    "u_fixed_mmc"            = rep(-5, ncol(X_fixed_mmc)),
    "u_fixed_tmc"            = rep(-5, ncol(X_fixed_tmc)),
    # age random effect
    "u_age_mmc"              = rep(0, ncol(X_age_mmc)),
    "u_age_tmc"              = rep(0, ncol(X_age_tmc)),
    # time random effect for MMC
    "u_time_mmc"             = rep(0, ncol(X_time_mmc)),
    # Space random effect (district)
    "u_space_mmc"            = rep(0, ncol(X_space_mmc)),
    "u_space_tmc"            = rep(0, ncol(X_space_tmc)),
    # Interactions for MMC
    # "u_agetime_mmc"          = matrix(0, ncol(X_age_mmc), ncol(X_time_mmc)),
    "u_agespace_mmc"         = matrix(0, ncol(X_age_mmc), ncol(X_space_mmc)),
    "u_spacetime_mmc"        = matrix(0, ncol(X_time_mmc), ncol(X_space_mmc)),
    # Interactions for TMC
    "u_agespace_tmc"         = matrix(0, ncol(X_age_tmc), ncol(X_space_tmc)),
    # Autocorrelation parameters for priors
    # Variance
    "logsigma_age_mmc"       = 0,
    "logsigma_time_mmc"      = 0,
    "logsigma_space_mmc"     = 0,
    # "logsigma_agetime_mmc"   = 0,
    "logsigma_agespace_mmc"  = 0,
    "logsigma_spacetime_mmc" = 0,
    "logsigma_age_tmc"       = 0,
    "logsigma_space_tmc"     = 0,
    "logsigma_agespace_tmc"  = 0,
    # Mean
    "logitrho_mmc_time1"     = 2,
    "logitrho_mmc_time2"     = 2,
    "logitrho_mmc_time3"     = 2,
    "logitrho_mmc_age1"      = 2,
    "logitrho_mmc_age2"      = 2,
    "logitrho_mmc_age3"      = 2,
    "logitrho_tmc_age1"      = 2,
    "logitrho_tmc_age2"      = 2
  )
)

fit <- threemc_fit_model(
  dat_tmb    = dat_tmb,
  mod        = mod,
  parameters = parameters,
  randoms    = c("u_time_mmc", "u_age_mmc", "u_space_mmc",
                 "u_agetime_mmc", "u_agespace_mmc", "u_spacetime_mmc",
                 "u_age_tmc", "u_space_tmc", "u_agespace_tmc"),
  N = N
)
# load("~/imperial_repos/threemc-orderly/bwa_zaf_fit.RData")

# subset to specific area level and calculate quantiles for rates and hazard
out_spec <- compute_quantiles(out, fit, area_lev = area_lev)
# out_spec <- readr::read_csv("~/imperial_repos/threemc-orderly/bwa_zaf_fit.csv.gz")

out_spec <- out_spec %>% 
  filter(!grepl("_zaf", area_id))

#### Removing Additional Country Data ####

#### Diagnostic Plots ####

pdf(paste0(save_dir, "Circ_Coverage.pdf"), width = 10)
ggplot(out_spec,
       aes(x = age,
           y = cum_incM,
           ymin = cum_incL,
           ymax = cum_incU,
           group = as.factor(year),
           colour = as.factor(year))) +
  geom_ribbon(fill = "lightgrey",
              colour = NA) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Age",
       y = "Coverage",
       colour = "") +
  theme_bw() +
  facet_wrap(. ~ area_name)
dev.off()

# Rates
pdf(paste0(save_dir, "Circ_Rates.pdf"), width = 10)
ggplot(out_spec,
       aes(x = age,
           y = rateM,
           ymin = rateL,
           ymax = rateU,
           group = as.factor(year),
           colour = as.factor(year))) +
  geom_ribbon(fill = "lightgrey",
              colour = NA) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Age",
       y = "Rates",
       colour = "") +
  theme_bw() +
  facet_wrap(. ~ area_name)
dev.off()

#### REMOVE ZAF DATA FROM OUT_SPEC & SAMPLE ####

# ?

#### saving results ####

# prepare for output
out_spec <- out_spec %>%
  select(
    area_id, area_name, area_level, year, age = circ_age,
    contains("obs"),
    cens, icens, N,
    contains("rate_mmc"), contains("rate_tmc"), contains("rate"),
    contains("surv"),
    contains("cum_inc_mmc"), contains("cum_inc_tmc"), contains("cum_inc"),
    contains("inc_mmc"), contains("inc_tmc"), contains("inc")
  )

# minimise fit object for saving
fit_save <- minimise_fit_obj(fit, dat_tmb, parameters)

# Saving results
data.table::fwrite(
  out_spec, file = paste0(save_dir, "Results_DistrictAgeTime_ByType.csv.gz")
)

# save fit as .rds file
saveRDS(fit_save, paste0(save_dir, "TMBObjects_DistrictAgeTime_ByType.rds"))
rm(fit_save); gc()

#### Aggregations ####

areas <- areas_orig %>% 
  # Add a unique identifier within Admin code and merging to boundaries
  sf::st_drop_geometry() %>%
  group_by(area_level) %>%
  mutate(space = row_number()) %>%
  ungroup()

# re-sample from model, if required (shouldn't be!)
if (is.null(fit$sample)) {
  fit <- threemc_fit_model(
    fit     = fit,
    mod     = mod,
    randoms = c("u_time_mmc", "u_age_mmc", "u_space_mmc",
                "u_agetime_mmc", "u_agespace_mmc", "u_spacetime_mmc",
                "u_age_tmc", "u_space_tmc", "u_agespace_tmc"),
    N       = 1000
  )
}

fit_no_prog <- fit
rm(fit); gc()

# want to aggregate for both discrete ages and "binned" age groups
age_vars <- list("inputs" = c("age", "age_group"), "names" = c("Age", "AgeGroup"))
# want to aggregate for various types
types <- c("probability", "incidence", "prevalence")

# run aggregations for each combination of age_vars and types
# aggregations <- lapply(seq_along(age_vars$inputs), function(i) {
lapply(seq_along(age_vars$inputs), function(i) {
  lapply(seq_along(types), function(j) {
    spec_results <- threemc_aggregate(
      .data       = out_spec,
      fit         = fit_no_prog,
      areas       = areas,
      populations = populations,
      age_var     = age_vars$inputs[[i]],
      type        = types[j],
      area_lev = area_lev,
      N = 100,
      prev_year = 2008 # year to compare with for prevalence
    )
    readr::write_csv(
      x = spec_results,
      file = paste0(
        save_dir, "Results_",
        age_vars$names[[i]], "_", stringr::str_to_title(types[j]),
        ".csv.gz"
      )
    )
    
    rm(spec_results); gc()
  })
})


