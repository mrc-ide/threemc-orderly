##############################################################################
# Run Model Calibration Tasks for all Countries and Parameter 
# Permutations On the Cluster 
##############################################################################

#### libs ####

library(orderly)
library(dplyr)
library(tidyr)
library(parallel)

#### Metadata ####

cores <- max(1, parallel::detectCores() - 1)

# task to run
task <- "val_hyperpar_temporal_prior_investigation3"
# task <- "02final_aggregations"
# task <- "01a_model_aggregate_spec_area_lev"
# check_task <- "01final_modelling"
check_task <- task

# objects to assign to 
# here we choose 
# - cluster to run on, 
# - cores to use if not mrc (large cluster), 
# - choice of TMBad or CppAD AD framework in threemc/TMB

# cluster_type <- "_4_tmbad"
# cluster_type <- "_4_cppad"
# cluster_type <- "_32_tmbad"
cluster_type <- "_32_cppad"
# cluster_type <- "_mrc_tmbad"
# cluster_type <- "_mrc_cppad"

# cluster run object
t_x <- paste0("t", cluster_type)
# individual cluster tasks
tasks_x <- paste0("tasks", cluster_type)
# specific parameter values 
pars_df_x <- paste0("pars_df", cluster_type)

# List of arguments to didehpc::didehpc_config
# TODO: Fill this in
is_mrc <- grepl("mrc", cluster_type) # check if running on large cluster
cores <- NULL
if (is_mrc == FALSE) cores = ifelse(grepl("4", cluster_type), 4, 32)
config_args <- list(
  # cluster to use (mrc is large cluster, > 1TB memory)
  cluster = ifelse(
    is_mrc,
    "mrc",
    "fi--didemrchnb"
  ),
  # template to use
  template = ifelse(is_mrc, "MEM1024", "32Core"),
  # if using 32Core template, how many cores to use (4 or 32) 
  cores = cores
)


#### Directory Setup ####

# directory to save contexts
# uses TMBad
if (grepl("tmbad", cluster_type)) {
  root <- "~/net/unaids-naomi/threemc-orderly/contexts_2"
# uses CppAD
} else {
  root <- "~/net/unaids-naomi/threemc-orderly/contexts_5"
}

# specify network share in didehpc::didehpc_config
config_args <- c(
  config_args,
  "workdir" = root
)

# ensure it exists, and if not, create it
threemc::create_dirs_r(root)

# orderly repos with tasks to be bundled
orderly_root <- here::here()

# path to save bundles
path_bundles <- file.path(root, "bundles")
threemc::create_dirs_r(path_bundles)


# output_path <- file.path(root, "output")
output_path <- "output"
# threemc::create_dirs_r(output_path)


#### Define parameter dataframe ####

# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" 
)
# no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD", "BFA", "COG") # ??
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
          "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
          "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
          "ZAF", "LBR", "GHA", "GMB", "NGA", "COD")
# Also need to remove countries for which there is only a single survey
single_year_iso3 <- c(
  "AGO", "CAF", "COD", "GAB", "GIN", "GMB", "NER", "SEN", "TGO"
)
iso3 <- iso3[!iso3 %in% c(single_year_iso3, vmmc_iso3, no_type_iso3)]

# Also remove countries which need to be run on pocatello (investigate!)
# iso3 <- iso3[!iso3 %in% c("LSO", "BDI")]

# for now, just do vmmc countries
# iso3 <- vmmc_iso3
# iso3 <- c("LSO", "SWZ") # just do some for now!
# iso3 <- c("RWA", "NAM", "ZMB")
# iso3 <- c("KEN", "ZAF", "MWI")
# iso3 <- "MOZ" # run while KEN, ZAF, MWI are pending
# iso3 <- c("ZWE", "UGA")
# iso3 <- c("UGA", "ETH", "TZA") # still to be run (running UGA on pocatello)
# iso3 <- c(
#   "SWZ", "RWA", "NAM", "ZMB", "KEN", 
#   "ZAF", "MWI", "MOZ", "TZA", "ETH", "UGA"
# )
# iso3 <- "TZA"
# iso3 <- vmmc_iso3[1:5]
# iso3 <- vmmc_iso3[6:9]
# iso3 <- vmmc_iso3[10:length(vmmc_iso3)]
# iso3 <- iso3[2:3]
# iso3 <- iso3[4:5] # CIV + MLI, all ran now!!
# iso3 <- iso3[6:7] # SLE + GHA, most failing, skipping for now! (still a lot failing, but not as many!)
# iso3 <- iso3[8] # NGA, try at least (may be v large) (good news is that's the last one!!)

# countries already completed
# complete_iso3 <- c("KEN", "MOZ", "NAM", "RWA", "ZAF", "ZMB", "LSO", "SWZ")
# iso3 <- iso3[!iso3 %in% complete_iso3]

# Model calibration: Do grid searches over time related variance hyperpars

# AR hyperpars (do a very course grid now, to begin with)
# ar_pars <- data.frame(
ar_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 0,
  # "logsigma_time_mmc"      = seq(0, 2.5, by = 0.25), 
  # "logsigma_time_mmc"      = seq(0, 3, by = 1.5), 
  "logsigma_time_mmc"      = seq(-1.5, 3, by = 1.5), 
  # "logsigma_agetime_mmc"   = seq(0, 2.5, by = 0.25), 
  # "logsigma_agetime_mmc"   = seq(0, 3, by = 1.5), 
  "logsigma_agetime_mmc"   = seq(0, 4.5, by = 1.5), 
  # "logsigma_spacetime_mmc" = seq(-1.5, 1, by = 0.25)
  # "logsigma_spacetime_mmc" = seq(-2, 1, by = 1.5)
  "logsigma_spacetime_mmc" = seq(-3.5, 1, by = 1.5)
)

# only run those we haven't run yet (for VMMC countries, at least)
# ar_pars <- ar_pars %>% 
#   filter(
#     !(cntry %in% vmmc_iso3 & 
#       (logsigma_time_mmc == -1.5 | 
#          logsigma_agetime_mmc == 4.5 | 
#            logsigma_spacetime_mmc == -3.5))
#   )


# RW 1 hyperpars
# rw_1_pars <- data.frame(
rw_1_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 1,
  # "logsigma_time_mmc"      = seq(-1, 1, by = 0.25), 
  # "logsigma_time_mmc"      = seq(-1, 1, by = 1), 
  "logsigma_time_mmc"      = seq(-1, 2, by = 1), 
  # "logsigma_agetime_mmc"   = seq(-1, 1, by = 0.25), 
  # "logsigma_agetime_mmc"   = seq(-1, 1, by = 1), 
  "logsigma_agetime_mmc"   = seq(-1, 2, by = 1), 
  # "logsigma_spacetime_mmc" = seq(-1, 1, by = 0.25)
  # "logsigma_spacetime_mmc" = seq(-1, 1, by = 1)
  "logsigma_spacetime_mmc" = seq(-2, 1, by = 1)
)

# rw_1_pars <- rw_1_pars %>% 
#   filter(
#     !(cntry %in% vmmc_iso3 & 
#     (logsigma_time_mmc == 2 | 
#        logsigma_agetime_mmc == 2 | 
#            logsigma_spacetime_mmc == -2))
#   )

# RW 2 hyperpars
# rw_2_pars <- data.frame(
rw_2_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 2,
  # "logsigma_time_mmc"      = seq(-2, 1,  by = 0.25), 
  # "logsigma_time_mmc"      = seq(-2, 1,  by = 1.5), 
  "logsigma_time_mmc"      = seq(-2, 2.5,  by = 1.5), 
  # "logsigma_agetime_mmc"   = seq(-4, -1, by = 0.25), 
  # "logsigma_agetime_mmc"   = seq(-4, -1, by = 1.5), 
  "logsigma_agetime_mmc"   = seq(-4, 0.5, by = 1.5), 
  # "logsigma_spacetime_mmc" = seq(-6, -3, by = 0.25)
  # "logsigma_spacetime_mmc" = seq(-6, -3, by = 1.5)
  "logsigma_spacetime_mmc" = seq(-6, -1.5, by = 1.5)
)

# rw_2_pars <- rw_2_pars %>% 
#   filter(
#     !(cntry %in% vmmc_iso3 & 
#     (logsigma_time_mmc == 2.5 | 
#        logsigma_agetime_mmc == 0.5 | 
#        logsigma_spacetime_mmc == -1.5))
#   )

pars_df <- bind_rows(
  ar_pars, 
  rw_1_pars, 
  rw_2_pars
)

# model spec: inc_time_tmc = TRUE for all w/ type, use paed_age_cutoff for vmmc

pars_df$paed_age_cutoff <- ifelse(
  pars_df$cntry %in% vmmc_iso3, 
  10, 
  Inf
)
pars_df <- pars_df %>% 
  mutate(
    paed_age_cutoff = ifelse(cntry %in% vmmc_iso3, 10, Inf), 
    inc_time_tmc    = ifelse(cntry %in% no_type_iso3, FALSE, TRUE)
  )

# double check model specification was done correctly
pars_df <- pars_df %>% 
  filter(
    (cntry %in% vmmc_iso3 & paed_age_cutoff == 10 & inc_time_tmc == TRUE)  |
      (cntry %in% no_type_iso3 & paed_age_cutoff == Inf & inc_time_tmc == FALSE) |
      (cntry %in% iso3 & paed_age_cutoff == Inf & inc_time_tmc == TRUE)
  )


#### Perform Orderly Search for Outstanding Tasks ####

# don't run already ran tasks
(names_ran <- unlist(mclapply(seq_len(nrow(pars_df)), function(i) {
  message(round(100 * (i / nrow(pars_df)), 3), "% completed")
  is_paper <- TRUE
  if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
    is_paper <- FALSE
  }
  orderly::orderly_search(
    name = check_task,
    query = "latest(
      parameter:cntry == cntry && parameter:is_paper == is_paper &&
      parameter:rw_order == rw_order && 
      parameter:logsigma_time_mmc == logsigma_time_mmc && 
      parameter:logsigma_agetime_mmc == logsigma_agetime_mmc && 
      parameter:logsigma_spacetime_mmc == logsigma_spacetime_mmc
    )",
    parameters = c(pars_df[i, ], "is_paper" = is_paper),
    root = orderly_root
  )
}, mc.cores = cores)))

pars_df <- pars_df[is.na(names_ran), ]

# Create pars_df object for specific cluster setup
assign(pars_df_x, pars_df)

# save for later
# TODO: fill in here


#### bundle orderly tasks #### 

# pack up task for each country
bundles <- mclapply(seq_len(nrow(pars_df)), function(i) {
  
  is_paper <- TRUE
  # if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
  #   is_paper <- FALSE 
  # }
  
  orderly::orderly_bundle_pack(
    path_bundles,
    check_task,
    parameters = c(as.list(pars_df[i, ]), "is_paper" = is_paper),
    root = orderly_root
  )
}, mc.cores = cores)

if (length(bundles[[1]]) != 2) {
  print(bundles[[1]])
  stop("Bundles incorrectly generated in mclappy")
}


#### contexts ####

# cluster config
# config <- didehpc::didehpc_config(
#   workdir = root,
#   # cluster = "fi--didemrchnb", 
#   cluster = "mrc", 
#   # template = "32Core",
#   template = "MEM1024"
#   # cores = 32
#   # cores = 4
# )
config <- do.call(didehpc::didehpc_config, config_args)

# setup context for orderly task (packages required, etc)
# use development versions of TMB and orderly (most recent)
package_sources <- c(
  "github::kaskr/adcomp/TMB",
  "github::vimc/orderly"
)
# use development version of threemc (TMBad or cppAD choice made in metadata)
if (grepl("cppad", cluster_type)) {
 package_sources <- c(package_sources, "github::mrc-ide/threemc@master_cppad")
} else {
 package_sources <- c(package_sources, "github::mrc-ide/threemc")
}

ctx <- context::context_save(
  path = root,
  packages = c(
    "orderly", "dplyr", "sf",
    "data.table", "rlang", "ggplot2"
  ),
  package_sources = conan::conan_sources(package_sources)
)

# queue above context on cluster
obj <- didehpc::queue_didehpc(
  context = ctx, 
  config = config # , 
  # provision = "upgrade"
)


#### run bundles ####

paths <- lapply(bundles, function(x) {
  paste(last(strsplit(dirname(x$path), "/")[[1]]), 
        basename(x$path), sep = "/")
})

# send orderly tasks to cluster! wait till they have all run before proceeding
# t <- obj$lapply(paths, orderly::orderly_bundle_run, workdir = output_path)
assign(
  t_x, 
  obj$lapply(paths, orderly::orderly_bundle_run, workdir = output_path)
)

# look at individual tasks for logs
# tasks <- t$tasks
# assign(tasks_x, t$tasks)
assign(tasks_x, get(t_x)$tasks)

# cancel jobs
# obj$unsubmit(t$ids)

tasks_run <- file.path(
  root, 
  "output",
  # vapply(t$X[t$status() == "COMPLETE"], basename, character(1))
  vapply(get(t_x)$X[get(t_x)$status() == "COMPLETE"], basename, character(1))
)

lapply(tasks_run, function(x) {
  system(paste0(
  # print(paste0(
    # "mv ", x, " ~/OneDrive/", task, "/zips_new/."
    "mv ", x, " ~/", task, "/zips_new/."
  ))
})

# check how many jobs have resulted in TMB errors
# running <- which(t$status() == "RUNNING")
# tmb_fail_tasks <- vapply(running, function(x) {
#   grepl("debugger", last(last(tasks[[x]]$log()$body)))
# }, logical(1))
# table(tmb_fail_tasks)


#### import completed tasks ####

# through orderly, won't work now unfortunately
for (output in t$wait(100)) {
  out <- strsplit(output$path, "\\\\")[[1]]
  output_filename <- out[length(out)]
  orderly::orderly_bundle_import(file.path(root, output_path, output_filename),
                                 root = orderly_root)
}
