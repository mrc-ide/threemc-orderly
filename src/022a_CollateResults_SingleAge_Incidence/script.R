#####################
### Preliminaries ###
#####################

# source aggregation functions
source("source.R")

# Number of samples to use
N <- 100

# country
# cntry <- "LSO"

# area hierarchy
area_lev <- threemc::datapack_psnu_area_level %>%
    filter(iso3 == cntry) %>%
    pull(psnu_area_level)

save_loc <- paste0("Results_Age_Incidence.csv.gz")

################################################
### Preparing location/shapefile information ###
################################################

# load shapefile
areas <- read_circ_data(
    path    = "areas.geojson",
    filters = c("iso3" = cntry)
    ) %>%
    # Add a unique identifier within Admin code and merging to boundaries
    sf::st_drop_geometry() %>%
    group_by(area_level) %>%
    mutate(space = row_number()) %>%
    ungroup()

# wide formatted areas, for changing area levels later
areas_wide <- areas %>%
  select(area_id, area_name, parent_area_id, area_level) %>%
  naomi::spread_areas()

# Load populations (for male pop for country in question only)
populations <- read_circ_data(
    "population_singleage.csv.gz",
    filters = c("iso3" = cntry, "sex" = "male")
)

# Model with Probability of MC
results <- read_circ_data("Results_DistrictAgeTime_ByType.csv.gz")
results$model <- "No program data"

# Load TMB model
compile_tmb("Surv_SpaceAgeTime_ByType.cpp")
dyn.load(TMB::dynlib("Surv_SpaceAgeTime_ByType"))

fit <- readRDS("TMBObjects_DistrictAgeTime_ByType.rds")

parlist <- split(fit$par.full, names(fit$par.full))[names(fit$par_init)]
is_matrix <- sapply(fit$par_init, is.matrix)
parlist[is_matrix] <- Map(matrix,
                          parlist[is_matrix],
                          nrow = lapply(fit$par_init[is_matrix], nrow),
                          ncol = lapply(fit$par_init[is_matrix], ncol))

fit$obj <- MakeADFun(data = fit$tmb_data,
                     parameters = parlist,
                     random = c("u_time_mmc",'u_age_mmc','u_space_mmc',
                                'u_agetime_mmc','u_agespace_mmc','u_spacetime_mmc',
                                'u_age_tmc','u_space_tmc','u_agespace_tmc'),
                     method = "BFGS",
                     hessian = TRUE,
                     DLL = "Surv_SpaceAgeTime_ByType")
fit$obj$fn()  
fit <- naomi::sample_tmb(fit, nsample = N)
fit_no_prog <- fit

rm(fit); gc()

# Model with total rate (i.e. including VMMC data)
# prog_results <- as_tibble(data.table::fread(here::here(
#   paste0("Runs/", cntry,
#          "_Results_DistrictAgeTime_ByType_withProgram_withBorrowing.csv.gz")
# )))
# prog_results$model <- "With program data"
# load(here::here(
#   paste0("Runs/", cntry,
#          "TMBObjects_DistrictAgeTime_ByType_withProgram_withBorrowing.RData"))
# )
# fit_prog <- fit # need to load model with programme data as well
# rm(fit); gc()

#########################################
### Loading rates from survival model ###
#########################################

results <- prepare_sample_data(N = N, # perhaps return number of NAs in population?
                               populations = populations,
                               no_prog_results = results,
                               no_prog_tmb_fit = fit_no_prog,
                               type = "incidence")

rm(populations, fit_no_prog); gc()

####################################
### Preparing results for output ###
####################################

# collect results for lower area hierarchies by joining higher area
# hierarchies (do so until you reach "area 0")
results <- combine_areas(results, area_lev, join = T)

# aggregate samples (any way to make this faster/less resource heavy?)
results <- aggregate_sample(results)

####################################
### Preparing results for output ###
####################################

# Getting medians and CIs
results <- posterior_summary_fun(results)

# Merging regional information on the dataset (i.e. parent area info)
results <- merge_area_info(results, areas)

####################
### Saving files ###
####################
# Saving files
data.table::fwrite(results, file = save_loc)