#### Preliminaries ####

# save loc
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir) # ensure save_dir exists; create if not

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
print(mod)

# re-sample from model
if (is.null(fit$sample)) {
    fit <- threemc_fit_model(
        fit     = fit,
        mod     = mod,
        randoms = c(
          "u_time_mmc", "u_age_mmc", "u_age_mmc_paed", "u_space_mmc",
          "u_agetime_mmc", "u_agespace_mmc", "u_agespace_mmc_paed",
          "u_spacetime_mmc", "u_age_tmc", "u_space_tmc", "u_agespace_tmc"
        ), 
        N       = N
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


#### Aggregating ####

# want to aggregate for both discrete ages and "binned" age groups
age_vars <- list("inputs" = c("age", "age_group"), "names" = c("Age", "AgeGroup"))
# want to aggregate for various
types <- c("probability", "incidence", "prevalence")

# run aggregations for each combination of age_vars and types
lapply(seq_along(age_vars$inputs), function(i) {
    lapply(seq_along(types), function(j) {
        spec_results <-  threemc_aggregate(
            .data       = results,
            fit         = fit_no_prog,
            areas       = areas,
            populations = populations,
            age_var     = age_vars$inputs[[i]],
            type        = types[j],
            area_lev = area_lev,
            N = N,
            prev_year = 2008 # year to compare with for prevalence
        )
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
