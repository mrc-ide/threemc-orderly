#### Posterior Predictive Check (Prevalence) ####

#### Initial ####

# directory to pull dependencies from
dir_path <- "depends/"
# save location
save_loc <- "artefacts/"
# ensure save loc exists
create_dirs_r(save_loc)

# age groups (should really make a 45-59 age group)
spec_age_groups <- c("15-29", "15-49", "25-49", "30-49", "50-54", "55-59")

# remove circumcisions with missing type?
rm_missing_type <- FALSE


#### Metadata to run the models ####

# set country
# cntry <- "MWI"

k_dt <- 5 # Age knot spacing
start_year <-  2006
if (cntry == "LBR") cens_age <- 29 else cens_age <- 59
N <- 1000

#### Reading in data ####

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# read in data, filter for specific country and male surveys only
filters <- c("iso3" = cntry, sex = "male")
areas <- read_circ_data(paste0(dir_path, "areas.geojson"), filters) %>%
  dplyr::mutate(space = 1:dplyr::n()) # add space column to areas
survey_circumcision <- read_circ_data(
  paste0(dir_path, "survey_circumcision.csv.gz"), filters
)
populations <- read_circ_data(
  paste0(dir_path, "population_singleage_aggr.csv.gz"), filters
)

# also take wide areas
areas_wide <- sf::st_drop_geometry(areas) %>% 
  dplyr::select(dplyr::all_of(
    c("area_id", "area_name", "parent_area_id", "area_level", "space")
  )) %>%
  spread_areas(space = FALSE)

# Load survey points
survey_estimates <- read_circ_data(
  paste0(dir_path, "survey-circumcision-coverage.csv.gz"), filters
) %>% 
  rename(
    year = survey_mid_calendar_quarter,
    mean = estimate,
    sd = std_error,
    lower = ci_lower,
    upper = ci_upper
  ) %>% 
  # change age group convention from YXXX_XXX to XX-XX
  change_agegroup_convention() %>% 
  # have indicator column match convention as well
  mutate(
    indicator = case_when(
      indicator == "circumcised"  ~ "MC",
      indicator == "circ_medical" ~ "MMC",
      TRUE                        ~ "TMC"
    )
  ) %>% 
  # only take desired age groups
  filter(age_group %in% spec_age_groups)

# pull recommended area hierarchy for target country
area_lev <- threemc::datapack_psnu_area_level %>%
  filter(iso3 == cntry) %>%
  pull(psnu_area_level)

# don't model at the country level
if (length(area_lev) > 0 && area_lev == 0) area_lev <- NULL

# if area_level is missing, assume most common area lev in surveys
if (length(area_lev) == 0) {
  area_lev <- table(as.numeric(substr(survey_circumcision$area_id, 5, 5)))
  area_lev <- as.numeric(names(area_lev)[area_lev == max(area_lev)])
}

#### remove most recent survey #### 

# If second last survey year is just one year previous, also remove it

# add survey year column
survey_circumcision <- survey_circumcision %>% 
  mutate(survey_year = as.numeric(substr(survey_id, 4, 7)))

# save original surveys, without removing most recent survey(s)
survey_circumcision_orig <- survey_circumcision

# find survey years, rm max year (and also second largest year, if appropriate)
survey_years <- sort(unique(survey_circumcision$survey_year))
max_years <- max(survey_years)
if (length(survey_years) > 1) {
  if ((max_years - survey_years[length(survey_years) - 1]) == 1) {
    max_years <- c(max_years, max_years - 1)
  }
  # record removed surveys
  removed_surveys <- survey_circumcision %>% 
    filter(survey_year %in% max_years) %>% 
    distinct(survey_id) %>% 
    pull()
  message("removed surveys: ", paste(removed_surveys, collapse = ", "))
  survey_circumcision <- survey_circumcision %>% 
    filter(!survey_year %in% max_years)
} else {
  # don't continue if there is only one survey available for a country
  stop(paste0(
    "Only one survey available for ", 
    cntry, 
    ", so OOS Validation not possible")
  )
}

# save used and unused surveys
used_surveys <- paste(unique(survey_circumcision$survey_id), collapse = ", ")
survey_info <- data.frame(
  "used_surveys" = used_surveys,
  "removed_surveys" = removed_surveys
)
readr::write_csv(survey_info, paste0(save_loc, "used_survey_info.csv"))

# pull year(s) from removed_surveys
removed_years <- as.numeric(substr(removed_surveys, 4, 7))


#### Preparing circumcision data ####

# perform following for both "OOS" surveys and all surveys
# survey_circs <- list(survey_circumcision, survey_circumcision_orig)
# 
# survey_circs <- lapply(survey_circs, function(x) {
#   # pull latest census year from survey_id (should I do this???)
#   cens_year <- max(as.numeric(
#     substr(unique(x$survey_id), 4, 7)
#   ))
#   
#   # Prepare circ data, and normalise survey weights and apply Kish coefficients.
#   x <- prepare_survey_data(
#     areas               = areas,
#     survey_circumcision = select(x, -matches("area_level")),
#     area_lev            = area_lev,
#     start_year          = start_year,
#     cens_year           = cens_year,
#     cens_age            = cens_age,
#     rm_missing_type     = rm_missing_type,
#     norm_kisk_weights   = TRUE
#   )
#   
#   if (nrow(x) == 0) {
#     message("no valid surveys at this level") # move inside function!
#   }
#   return(x)
# })

# Prepare circ data, and normalise survey weights and apply Kish coefficients
cens_year <- max(as.numeric(
  substr(unique(survey_circumcision$survey_id), 4, 7)
))

# Prepare circ data, and normalise survey weights and apply Kish coefficients.
survey_circumcision <- prepare_survey_data(
      areas               = areas,
      survey_circumcision = select(survey_circumcision, -matches("area_level")),
      area_lev            = area_lev,
      start_year          = start_year,
      cens_year           = cens_year,
      cens_age            = cens_age,
      rm_missing_type     = rm_missing_type,
      norm_kisk_weights   = TRUE
    )

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

# Skeleton dataset for both
out <- create_shell_dataset(
  survey_circumcision = survey_circumcision,
  population_data     = populations,
  areas               = areas,
  area_lev            = area_lev,
  time1               = "time1",
  time2               = "time2",
  strat               = "space",
  age                 = "age",
  circ                = "indweight_st"
)


#### Dataset for modelling ####

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
if (is_type == TRUE) {
  mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
} else mod <- "Surv_SpaceAgeTime"

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
    "u_agetime_mmc"          = matrix(0, ncol(X_age_mmc), ncol(X_time_mmc)),
    "u_agespace_mmc"         = matrix(0, ncol(X_age_mmc), ncol(X_space_mmc)),
    "u_spacetime_mmc"        = matrix(0, ncol(X_time_mmc), ncol(X_space_mmc)),
    # Interactions for TMC
    "u_agespace_tmc"         = matrix(0, ncol(X_age_tmc), ncol(X_space_tmc)),
    # Autocorrelation parameters for priors
    # Variance
    "logsigma_age_mmc"       = 0,
    "logsigma_time_mmc"      = 0,
    "logsigma_space_mmc"     = 0,
    "logsigma_agetime_mmc"   = 0,
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
# saveRDS(fit, "~/imperial_repos/threemc-orderly/swz_fit_temp.RDS")
# saveRDS(fit, paste0("~/imperial_repos/threemc-orderly/", cntry, "_fit_temp.RDS"))

# fit <- readRDS("~/imperial_repos/threemc-orderly/swz_fit_temp.RDS")
# fit <- readRDS(paste0("~/imperial_repos/threemc-orderly/", cntry, "_fit_temp.RDS"))

# subset to specific area level and calculate quantiles for rates and hazard
# out_spec <- compute_quantiles(out, fit, area_lev = area_lev)

#### Joining Samples with Results ####

# filter to area level modelled 
out <- out %>%
  dplyr::filter(.data$area_level == area_lev)

# pull 1000 samples for each desired output from fit
samples <- fit$sample
# only want "cum_inc" or rates 
samples <- samples[grepl("cum_inc", names(samples))]
# ensure names for MC columns in fit have the suffix "_mc"
samples <- threemc:::append_mc_name(samples)
# replace "cum_inc"
names(samples) <- toupper(stringr::str_replace_all(names(samples), "cum_inc_", ""))


#### Split out between types ####

# split between following types
types_out <- c("MC", "MMC", "TMC")

# select identification cols & rate col of interest and join with samples
out_types <- lapply(types_out, function(x) {
  out_spec <- select(out, area_id:population) # do I need pop? Maybe if aggregating
  n <- length(out_spec)
  out_spec[, (n +1):(n + 1000)] <- samples[[x]]
  out_spec <- out_spec %>% 
    mutate(indicator = x)
    # filter(out_spec, year %in% removed_years)
}) %>% 
  bind_rows() %>% 
  # only take years of interest, in which surveys were removed from model data
  filter(year %in% removed_years, area_level == area_lev) # also modelled lev

#### Add additional area levels ####

# if (all(as.numeric(substr(out_types$area_id, 5, 5)) > 1)) {
#   
#   keep_cols <- c("indicator", "circ_age", names(out_types)[grepl("^V", names(out_types))])
#   
#   # collect data for lower area hierarchies by joining higher area
#   # hierarchies (until you reach "area 0")
#   out_types <- threemc:::combine_areas(
#     out_types, 
#     areas_wide, 
#     area_lev, 
#     add_keep_cols = keep_cols,
#     join = FALSE
#   )
# } else out_types <- list(out_types)
out_types <- list(out_types)


#### Aggregate to Age Groups #### 

# change col names to work in aggregate_sample_age_group
out_types <- lapply(out_types, function(x) {
  names(x)[grepl("V", names(x))] <- paste0("samp_", 1:1000)
  return(rename(x, type = indicator))
})

out_types_agegroup <- threemc:::aggregate_sample_age_group(
  out_types,
  aggr_cols = c(
    "area_id", "area_name", "area_level", "year", "type"
  ), 
  age_groups = spec_age_groups, 
  N = 1000
) %>% 
  rename(indicator = type) %>% # rename to match survey points df
  relocate(age_group, .before = samp_1) %>% # have sample cols last 
  filter(area_level == 1) # filter for area level 1 only


#### Prepare Survey Points & Join with Samples ####

survey_estimates_prep <- survey_estimates %>% 
  # filter for OOS year(s) and modelled area_level 
  # TODO: Could also aggregate up to other area levels for out_types_agegroup??
  filter(
    year %in% removed_years, 
    area_level == 1,
    age_group %in% out_types_agegroup$age_group,
    mean != 0
  ) %>% 
  # ignore survey_id, merging for the same year
  group_by(area_id, year, age_group, indicator) %>% 
  summarise(mean = sum(mean), .groups = "drop") 

# join with samples
survey_estimates_ppd <- survey_estimates_prep %>% 
  left_join(out_types_agegroup)

# save for later analysis
readr::write_csv(survey_estimates_ppd, "artefacts/ppd.csv.gz")

#### Calculating Posterior Predictive Check for Prevalence Estimations ####

# function to calculate where in model sample prevalence dist empirical rates are
# quant_pos_sum <- function(y, x) if (y < x) 0 else 1
# 
# survey_estimates_ppd_dist <- survey_estimates_ppd %>% 
#   relocate(mean, .before = samp_1) %>% 
#   group_by(across(area_id:area_level)) %>% 
#   rowwise() %>% 
#  summarise(
#   quant_pos = sum(across(starts_with("samp_"), ~quant_pos_sum(
#     mean, .x
#   ))),
#   .groups = "drop"
#  )
# 
# # Investigate 0s 
# survey_estimates_ppd %>% 
#   semi_join(filter(survey_estimates_ppd_dist, quant_pos == 0)) %>% 
#   select(mean, contains("samp")) %>% 
#   tidyr::pivot_longer(cols = contains("samp")) %>% 
#   filter(mean > value)  
# 
# # investigate 1000s (lots for ZAF!)
# survey_estimates_ppd %>% 
#   semi_join(filter(survey_estimates_ppd_dist, quant_pos == 1000)) %>% 
#   select(mean, contains("samp")) %>% 
#   tidyr::pivot_longer(cols = contains("samp")) %>% 
#   filter(mean < value)  

# No rows!! What could I be doing wrong here??? Or does my model just genuinely 
# poor fit for SWZ at Region level?? Consult Shiny app!
# Seems that OOS fit is quite good for younger ages, but declines for older people 
# Check TMC and MMC too!
