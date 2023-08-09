##############################################################################
# Run Modelling/Aggregation Tasks for all Countries and Parameter 
# Permutations On the Cluster 
##############################################################################

#### libs ####

library(orderly)
library(dplyr)
library(tidyr)

#### Metadata ####

# task to run
task <- "01final_modelling"
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
  root <- "~/net/unaids-naomi/threemc-orderly/contexts"
# uses CppAD
} else {
  root <- "~/net/unaids-naomi/threemc-orderly/contexts_4"
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

# countries remaining to be modelled
# remaining_iso3 <- c(
#   "ETH", "MOZ", "TZA", "UGA", 
#   "CIV", "CMR", "GAB", "GHA", "NGA"
# )
# remaining_iso3 <- c(
#   "ETH", "MOZ", "TZA", "UGA", "ZMB", 
#   "CIV", "CMR", "GAB", "GHA", "NGA"
# )


# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" # LSO, SWZ ran already
)
# vmmc_iso3 <- vmmc_iso3[vmmc_iso3 %in% remaining_iso3]
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
          "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
          "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
          "ZAF", "LBR", "GHA", "GMB", "NGA", "COD")
iso3 <- iso3[!iso3 %in% vmmc_iso3]
# iso3 <- iso3[iso3 %in% c(remaining_iso3, vmmc_iso3)]

rw_order <- c(0, 1, 2)
paed_age_cutoff <- c(10, Inf)
inc_time_tmc <- c(FALSE, TRUE)

# parameters for vmmc countries 
vmmc_par_df <- crossing(
  cntry = vmmc_iso3, rw_order, paed_age_cutoff, inc_time_tmc
  # cntry = vmmc_iso3, rw_order = 0, paed_age_cutoff = 10, inc_time_tmc = TRUE
)
# vmmc_par_df[vmmc_par_df$cntry %in% c("KEN", "ETH"), ]$paed_age_cutoff <- Inf
pars_df <- bind_rows(
  vmmc_par_df,
  crossing(
    # cntry = iso3, rw_order = 0, paed_age_cutoff = Inf, inc_time_tmc = TRUE
    cntry = iso3, 
    rw_order = rw_order, 
    paed_age_cutoff = paed_age_cutoff, 
    inc_time_tmc = inc_time_tmc
  )
)

pars_df <- pars_df %>% 
  mutate(
    # paed_age_cutoff = ifelse(cntry %in% c(no_type_iso3, "KEN", "ETH"), Inf, paed_age_cutoff), 
    inc_time_tmc    = ifelse(cntry %in% (no_type_iso3), FALSE, inc_time_tmc),
    paed_age_cutoff = ifelse(cntry %in% (no_type_iso3), Inf, paed_age_cutoff)
) %>% 
  distinct()

# pars_df <- pars_df[seq_len(nrow(pars_df) / 2), ]

# run national level model fits
if (task == "01a_model_aggregate_spec_area_lev") {
  pars_df$area_lev <- 0
}

# pars_df <- pars_df %>% 
#   filter(cntry == "TZA", rw_order == 0, paed_age_cutoff == 10)

# set.seed(123)
# pars_df <- pars_df[unique(round(runif(5, 1, nrow(pars_df)))), ]
# pars_df <- pars_df %>% 
#   distinct(rw_order, paed_age_cutoff, inc_time_tmc) %>% 
#   mutate(cntry = "LSO")
# pars_df <- pars_df %>% 
#   filter(cntry == "ETH", rw_order == 1, inc_time_tmc == TRUE)

# pars_df <- pars_df[1, ]
pars_df <- readr::read_csv("01final_modelling_remaining_tasks.csv")

#### Perform Orderly Search for Outstanding Tasks ####

names_ran <- (unlist(lapply(seq_len(nrow(pars_df)), function(i) {
  message(100 * (i / nrow(pars_df)), "% completed")
  is_paper <- TRUE
  # if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
  #   is_paper <- FALSE
  # }
    
  # print(is_paper)
  
  orderly::orderly_search(
    name = check_task,
    # query = "(
    query = "latest(
      parameter:cntry == cntry && parameter:is_paper == is_paper &&
      parameter:rw_order == rw_order && parameter:paed_age_cutoff == paed_age_cutoff
      && parameter:inc_time_tmc == inc_time_tmc
    )",
    parameters = list(
      cntry                  = pars_df$cntry[i],
      is_paper               = is_paper,
      rw_order               = pars_df$rw_order[i],
      paed_age_cutoff        = pars_df$paed_age_cutoff[i],
      inc_time_tmc           = pars_df$inc_time_tmc[i]
    ),
    root = orderly_root
  )
})))

pars_df <- pars_df[is.na(names_ran), ]

# run tasks that seem to run out of memory on the 
# "low memory" cluster
pars_df <- pars_df %>%
  # filter(cntry %in% c("GHA", "TZA", "UGA", "MOZ")) %>%
  # filter(!cntry %in% c("GHA", "TZA", "UGA", "MOZ")) %>%
  # filter(cntry %in% "GAB") %>% 
  # filter(rw_order == 0) %>% 
  arrange(cntry) %>% 
  # remove these parameters for testing 01_modelling
  # select(-c("rw_order", "paed_age_cutoff", "inc_time_tmc"))
  identity()

# Create pars_df object for specific cluster setup
assign(pars_df_x, pars_df)

# save for later
# readr::write_csv(pars_df, "01final_modelling_remaining_tasks.csv")

#### bundle orderly tasks #### 

# pack up task for each country
# pars_df <- pars_df[c(1, 10), ]
bundles <- lapply(seq_len(nrow(pars_df)), function(i) {
  
  is_paper <- TRUE
  # if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
  #   is_paper <- FALSE 
  # }
  
  orderly::orderly_bundle_pack(
    path_bundles,
    task,
    parameters = c(as.list(pars_df[i, ]), "is_paper" = is_paper),
    root = orderly_root
  )
})

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
    "dplyr", "sf",
    "data.table", "rlang", "ggplot2", "memuse"
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
