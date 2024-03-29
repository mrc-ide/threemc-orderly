#### Preliminaries ####

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

# age groups to aggregate for
age_groups <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
  "35-39", "40-44", "45-49", "50-54", "54-59", "0+", "10+", 
  "15+", "0-14", "10-24", "15-24", "10-29", "15-29", "10-39", 
  "15-39", "10-49", "15-49", "30-49"
)

# split age groups into 3 to run three times
age_group_splits <- split(
  seq_along(age_groups), sort(seq_along(age_groups)) %% 3
)

#### Preparing location/shapefile information ####

# load shapefile
areas <- read_circ_data(
  path    = "depends/areas.geojson",
  filters = c("iso3" = cntry)
) %>%
  # Add a unique identifier within Admin code and merging to boundaries
  sf::st_drop_geometry() %>%
  group_by(area_level) %>%
  mutate(space = row_number()) %>%
  ungroup()

# Load populations (for male pop for country in question only)
populations <- read_circ_data(
  "depends/population_singleage_aggr.csv.gz",
  filters = c("iso3" = cntry, "sex" = "male")
)

# Model with Probability of MC
results <- read_circ_data("depends/Results_DistrictAgeTime_ByType.csv.gz")
results$model <- "No program data"
# "small" model fit object 
fit <- readRDS("depends/TMBObjects_DistrictAgeTime_ByType.rds")
# 
mod_selector <- function(
    inc_type,  
    rw_order        = NULL, 
    paed_age_cutoff = NULL, 
    inc_time_tmc    = NULL
) {
  mod <- "Surv_SpaceAgeTime"
  if (!is.null(inc_type) && !is.na(inc_type) && inc_type == TRUE) {
    mod <- paste0(mod, "_ByType_withUnknownType")
  }
  paed_cond <- !is.null(paed_age_cutoff) && 
    !is.na(paed_age_cutoff) && 
    !is.infinite(paed_age_cutoff) && 
    inc_type == TRUE 
  if (paed_cond) {
    mod <- paste0(mod, "_Const_Paed_MMC")
  }
  if (!is.null(rw_order) && !is.na(rw_order) && rw_order %in% c(1, 2)) {
    mod <- paste0(mod, "_RW")
    if (inc_type == FALSE) return(mod)
  }
  if (!is.null(inc_time_tmc) && !is.na(inc_time_tmc) && inc_time_tmc == TRUE) {
    mod <- paste0(mod, 2)
  }
  return(mod)
}

if (all(results$obs_mmc == 0 & results$obs_tmc == 0)) {
  inc_type = FALSE
} else {
  inc_type = TRUE
}

mod <- mod_selector(inc_type, rw_order, paed_age_cutoff, inc_time_tmc)
message(paste0("mod == ", mod))

# re-sample from model
if (is.null(fit$sample)) {
  fit <- threemc_fit_model(
    fit     = fit,
    mod     = mod,
    randoms = c(
      "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
      "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
      "u_spacetime_mmc",
      "u_time_tmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
    ),
    N       = 1000
  )
}

fit_no_prog <- fit
rm(fit); gc()

# area hierarchy
area_lev <- threemc::datapack_psnu_area_level %>%
  filter(iso3 == cntry) %>%
  pull(psnu_area_level)

# don't model at the country level
if (length(area_lev) > 0 && area_lev == 0) area_lev <- NULL

# if area_level is missing (or 0), assume most common area lev in results
if (length(area_lev) == 0) {
  area_lev <- table(as.numeric(substr(results$area_id, 5, 5)))
  area_lev <- as.numeric(names(area_lev)[area_lev == max(area_lev)])
}

# Temp: use area level 2 for SWZ
# if (cntry == "SWZ") area_lev <- 2


#### Aggregating ####

# want to aggregate for both discrete ages and "binned" age groups
age_vars <- list("inputs" = c("age", "age_group"), "names" = c("Age", "AgeGroup"))
# age_vars <- list("inputs" = c("age_group"), "names" = c("AgeGroup"))
# age_vars <- list("inputs" = c("age"), "names" = c("Age"))
# want to aggregate for various
types <- c("probability", "incidence", "prevalence")

# run aggregations for each combination of age_vars and types
lapply(seq_along(age_vars$inputs), function(i) {
  lapply(seq_along(types), function(j) {
    # split age groups calculations to avoid large vectors
    if (cntry %in% c("GHA", "COD") && age_vars$inputs[[i]] == "age_group") {
      spec_results <- bind_rows(lapply(age_group_splits, function(x) {
        threemc_aggregate(
          .data       = results,
          fit         = fit_no_prog,
          areas       = areas,
          populations = populations,
          age_var     = age_vars$inputs[[i]],
          type        = types[j],
          area_lev    = area_lev,
          N           = N,
          prev_year   = 2006, # year to compare with for prevalence
          age_groups = age_groups[x]
        )
      })) %>% 
        arrange(area_id, year, type, age_group)
      
    } else {
      spec_results <-  threemc_aggregate(
        .data       = results,
        fit         = fit_no_prog,
        areas       = areas,
        populations = populations,
        age_var     = age_vars$inputs[[i]],
        type        = types[j],
        area_lev    = area_lev,
        N           = N,
        prev_year   = 2006 # year to compare with for prevalence
      )
    }
    readr::write_csv(
      x = spec_results,
      file = paste0(
        save_dir, "Results_", age_vars$names[[i]], "_", 
        stringr::str_to_title(types[j]), ".csv.gz"
      )
    )
    rm(spec_results); gc()
    message(paste0("Completed ",  
                   "Results_", age_vars$names[[i]], "_", 
                   stringr::str_to_title(types[j])))
  })
})
