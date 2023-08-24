##############################################################################
# Run Posterior Predictive Check Tasks for all Countries and Parameter 
# Permutations On the Cluster 
##############################################################################

#### libs ####

library(orderly)
library(dplyr)
library(tidyr)
library(parallel)

#### Metadata ####

n_cores <- max(1, parallel::detectCores() - 1)

# task to run
# task <- "01efinal_ppc_models"
task <- "01e_new_final_ppc_models"
# task <- "01e2final_mod_and_ppc"

# objects to assign to 
# here we choose 
# - cluster to run on, 
# - cores to use if not mrc (large cluster), 
# - choice of TMBad or CppAD AD framework in threemc/TMB

# cluster_type <- "_4_tmbad"
# cluster_type <- "_4_cppad"
# cluster_type <- "_8_tmbad"
# cluster_type <- "_8_cppad"
# cluster_type <- "_12_tmbad"
# cluster_type <- "_12_cppad"
# cluster_type <- "_16_tmbad"
# cluster_type <- "_16_cppad"
cluster_type <- "_32_tmbad"
# cluster_type <- "_32_cppad"
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
cluster_cores <- NULL
if (is_mrc == FALSE) {
  cluster_cores = case_when(
    grepl("4", cluster_type)  ~ 4, 
    grepl("8", cluster_type)  ~ 8, 
    grepl("16", cluster_type) ~ 16, 
    TRUE                      ~ 32
  )
}

config_args <- list(
  # cluster to use (mrc is large cluster, > 1TB memory)
  "cluster" = ifelse(
    is_mrc,
    "mrc",
    "fi--didemrchnb"
  ),
  # template to use
  "template" = ifelse(is_mrc, "MEM1024", "32Core"),
  # if using 32Core template, how many cores to use (4 or 32) 
  "cores" = cluster_cores
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
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" # LSO, SWZ ran already
)
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
# all SSA countries
iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
          "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
          "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
          "ZAF", "LBR", "GHA", "GMB", "NGA", "COD")
iso3 <- iso3[!iso3 %in% c(vmmc_iso3, no_type_iso3)]
# iso3 <- iso3[!iso3 %in% no_type_iso3]

rw_order <- c(0, 1, 2)
paed_age_cutoff <- c(Inf, 10)
inc_time_tmc <- c(FALSE, TRUE)

# parameters for vmmc countries 
vmmc_par_df <- crossing(
  cntry = vmmc_iso3, 
  rw_order, 
  paed_age_cutoff, 
  inc_time_tmc
)
# vmmc_par_df <- data.frame(
#   cntry           = vmmc_iso3,
#   rw_order        = 0,
#   paed_age_cutoff = 10,
#   inc_time_tmc    = TRUE
# )

# vmmc_par_df <- crossing(
#   cntry = c("MWI", "ZAF", "ZMB", "TZA", "ETH"), 
#   rw_order, 
#   paed_age_cutoff, 
#   inc_time_tmc
# )

# pars_df <- vmmc_par_df
# pars_df <- crossing(
#   cntry = iso3, rw_order, paed_age_cutoff, inc_time_tmc
# )

pars_df <- bind_rows(
  vmmc_par_df,
  crossing(cntry = iso3, rw_order, paed_age_cutoff, inc_time_tmc)
  # data.frame(cntry           = iso3,
  #            rw_order        = 0,
  #            paed_age_cutoff = Inf,
  #            inc_time_tmc    = TRUE)
)

# add countries with no type (running on pocatello)
pars_df <- bind_rows(
  pars_df,
  # data.frame(
  crossing(
    cntry = no_type_iso3,
    rw_order = rw_order,
    # rw_order        = 0,
    paed_age_cutoff = Inf,
    inc_time_tmc    = FALSE
  )
)


#### Perform Orderly Search for Outstanding Tasks ####

# dont run for no-type countries
pars_df <- pars_df %>% 
  filter(!cntry %in% no_type_iso3)

# only run for parameters with successful models
if (task != "01e2final_mod_and_ppc") {
  (names <- (unlist(mclapply(seq_len(nrow(pars_df)), function(i) {
    
    is_paper <- TRUE
    # if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
    #   is_paper <- FALSE
    # }
   
    # return progress message
    system(sprintf(
      'echo "\n%s\n"', 
      paste0(100 * (i / nrow(pars_df)), "% completed", collapse = "")
    ))
    
    orderly::orderly_search(
      name = "01final_modelling",
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
  }, mc.cores = n_cores))))
  
  pars_df <- pars_df[!is.na(names), ]
}

# run 01final_modelling tasks that are remaining
# pars_df <- pars_df[is.na(names), ]

# only run for tasks which haven't been run before 
rm_ran_tasks <- function(pars_df, check_task) {
  (names_ran <- (unlist(mclapply(seq_len(nrow(pars_df)), function(i) {
    message(100 * (i / nrow(pars_df)), "% completed")
    is_paper <- TRUE
    # if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
    #   is_paper <- FALSE
    # }
    
    # return progress message
    system(sprintf(
      'echo "\n%s\n"', 
      paste0(100 * (i / nrow(pars_df)), "% completed", collapse = "")
    ))
    
    orderly::orderly_search(
      name = check_task,
      query = "latest(
        parameter:cntry == cntry &&
        parameter:is_paper == is_paper &&
        parameter:rw_order == rw_order &&
        parameter:paed_age_cutoff == paed_age_cutoff &&
        parameter:inc_time_tmc == inc_time_tmc
      )",
      parameters = list(
        cntry                  = pars_df$cntry[i],
        is_paper               = is_paper,
        rw_order               = pars_df$rw_order[i],
        paed_age_cutoff        = pars_df$paed_age_cutoff[i],
        # area_lev               = pars_df$area_lev[i],
        inc_time_tmc           = pars_df$inc_time_tmc[i]
      ),
      root = orderly_root
    )
  }, mc.cores = n_cores))))
  pars_df <- pars_df[is.na(names_ran), ]
}

pars_df <- rm_ran_tasks(pars_df, task)
# if running mod + ppc task, also check for ran PPCs
check_tasks <- c(
  "01e_new_final_ppc_models", 
  "01e2final_mod_and_ppc"
)
add_check_task <- setdiff(check_tasks, task)
if (length(add_check_task) == 1) {
  pars_df <- rm_ran_tasks(pars_df, add_check_task)
}

# Create pars_df object for specific cluster setup
assign(pars_df_x, pars_df)

# save for later
# readr::write_csv(
#   pars_df, 
#   "remaining_tasks/01e_new_final_ppc_models.csv"
# )


#### bundle orderly tasks #### 

# pack up task for each country
bundles <- mclapply(seq_len(nrow(pars_df)), function(i) {
  
  is_paper <- TRUE
  # if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
  #   is_paper <- FALSE 
  # }
  
  # return progress message
  system(sprintf(
    'echo "\n%s\n"', 
    paste0(100 * (i / nrow(pars_df)), "% completed", collapse = "")
  ))
  
  orderly::orderly_bundle_pack(
    path_bundles,
    task,
    parameters = c(as.list(pars_df[i, ]), "is_paper" = is_paper),
    root = orderly_root
  )
}, mc.cores = n_cores)

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
    "dplyr", "sf", "Matrix", "R.utils", "spdep", "scoringutils",
    "loo", "data.table", "rlang", "ggplot2", "memuse", "readr"
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

run_tasks <- file.path(
  root, 
  "output",
  # vapply(t$X[t$status() == "COMPLETE"], basename, character(1))
  vapply(get(t_x)$X[get(t_x)$status() == "COMPLETE"], basename, character(1))
)

lapply(run_tasks, function(x) {
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