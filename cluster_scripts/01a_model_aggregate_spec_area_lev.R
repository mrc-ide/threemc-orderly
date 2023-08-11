#### Run National Level model fits ####

#### Libs ####

library(dplyr)
library(tidyr)
library(orderly)
library(parallel)


#### Metadata & parameter dataframe definition ####

# task to run
task <- "01a_model_aggregate_spec_area_lev"

n_splits <- 3
n <- 1
n_cores <- detectCores() - 1

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

# rw_order <- c(0, 1, 2)
# paed_age_cutoff <- c(10, Inf)
# inc_time_tmc <- c(FALSE, TRUE)

# parameters for VMMC countries 
vmmc_par_df <- crossing(
  cntry = vmmc_iso3, 
  # rw_order, paed_age_cutoff, inc_time_tmc
  rw_order = 0, paed_age_cutoff = 10, inc_time_tmc = TRUE
)

pars_df <- bind_rows(
  vmmc_par_df,
  # parameters for non-VMMC countries
  crossing(
    cntry = iso3, 
    rw_order = 0, paed_age_cutoff = Inf, inc_time_tmc = TRUE
    # rw_order, paed_age_cutoff, inc_time_tmc
  )
)

pars_df <- pars_df %>% 
  mutate(
    # paed_age_cutoff = ifelse(cntry %in% c(no_type_iso3, "KEN", "ETH"), Inf, paed_age_cutoff), 
    # parameters for countries with no type information
    inc_time_tmc    = ifelse(cntry %in% (no_type_iso3), FALSE, inc_time_tmc),
    paed_age_cutoff = ifelse(cntry %in% (no_type_iso3), Inf, paed_age_cutoff)
  ) %>% 
  distinct()

# don't repeat previously run tasks
names_ran <- unlist(mclapply(seq_len(nrow(pars_df)), function(i) {
  message(round(100 * (i / nrow(pars_df)), 3), "% completed")
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
    name = task,
    query = "latest(
      parameter:cntry == cntry && 
      parameter:rw_order == rw_order && 
      parameter:paed_age_cutoff == paed_age_cutoff &&
      parameter:inc_time_tmc == inc_time_tmc
    )",
    parameters = c(pars_df[i, ], "is_paper" = is_paper)
  )
}, mc.cores = n_cores))

pars_df <- pars_df[is.na(names_ran), ]

#### Run tasks ####

# run in batches
pars_splits <- split(
  seq_len(nrow(pars_df)),
  sort(seq_len(nrow(pars_df))) %% n_splits
)

pars_df <- pars_df[pars_splits[[n]], ]

# run models
orderly::orderly_batch(
  name = task, 
  parameters = pars_df, 
  continue_on_error = TRUE
)
