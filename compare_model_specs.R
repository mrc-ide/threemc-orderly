#### Pull together fit statistics for different model specs ####

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

#### Parameter Values ####

# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" 
)
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
          "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
          "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
          "ZAF", "LBR", "GHA", "GMB", "NGA", "COD")
# iso3 <- iso3[!iso3 %in% no_type_iso3]
iso3 <- iso3[!iso3 %in% c(vmmc_iso3, no_type_iso3)]

# TODO: test with VMMC countries
# iso3 <- vmmc_iso3

pars_df <- tidyr::crossing(
  # "cntry" = iso3,
  cntry = c(iso3, vmmc_iso3, no_type_iso3),
  "rw_order" = c(0, 1, 2),
  "paed_age_cutoff" = c(10, Inf),
  "inc_time_tmc" = c(FALSE, TRUE)
)

# uncomment to only look at models we are currently using
pars_df <- pars_df %>%
  filter(
      rw_order == 0,
      (cntry %in% no_type_iso3 & paed_age_cutoff == Inf & inc_time_tmc == FALSE) |
      (cntry %in% vmmc_iso3 & paed_age_cutoff == 10 & inc_time_tmc == TRUE) |
      (cntry %in% iso3 & paed_age_cutoff == Inf & inc_time_tmc == TRUE)
 )

# pars_df <- pars_df %>% 
#   filter(cntry == "ZAF")


#### Function to load orderly data ####

# takes a data frame of parameters and performs an orderly search on each row.
# By default, can also load these files (also ran parallel)
load_orderly_data <- function(
    # parameters fed to orderly::orderly_search
    task,
    parameters = NULL, # needs to be a df, not a list!
    query = NULL, 
    filenames = NULL, # name of specific artefact file to load, if desired
    dirs = NULL, # optionally just provide dirs to skip orderly_search
    load_fun = readr::read_csv, # function to load data with, if so desired
    ncores = max(1, parallel::detectCores() - 2) 
  ) {
  
  # check that either parameters & query or just dirs have been provided
  # (could also add a query parser here for the parameters!)
  stopifnot((!is.null(parameters) & !is.null(query)) || !is.null(dirs))
  
  if (is.null(dirs)) {
    # search parameter space specified for previously run orderly tasks
    dirs <- unlist(parallel::mclapply(seq_len(nrow(parameters)), function(i) {
      # give progress (no longer works properly w/ mclapply rather than lapply)
      # message(100 * (i / nrow(parameters)), "% completed") 
      system(sprintf(
        'echo "\n%s\n"', 
        paste0(100 * (i / nrow(parameters)), "% completed", collapse = "")
      ))
      orderly::orderly_search(
        query = query, 
        name = task, 
        parameters = c(parameters[i, ]) # coerces pars df to a list 
      )
    }, mc.cores = ncores))  
  }
  
  # return NAs in parameters search, but only load files from found directories
  dirs_return <- dirs
  dirs <- dirs[!is.na(dirs)]
  # return dirs if filenames unspecified
  if (is.null(filenames)) return(list("dirs" = dirs_return))
  files <- file.path(
    "archive", 
    task,
    dirs, 
    "artefacts/", # prob don't need this? I just structure my tasks this way
    filenames
  )
  # return filenames if load_fun isn't specified
  if (!is.null(load_fun) == FALSE) return(files)
  return(list(
    "dirs" = dirs_return, 
    "output" = lapply(files, load_fun)
  ))
}

fit_stats <- load_orderly_data(
  task = "01efinal_ppc_models", 
  parameters = pars_df,
  query = "latest(
      parameter:cntry                   == cntry && 
      parameter:rw_order                == rw_order &&
      parameter:paed_age_cutoff         == paed_age_cutoff &&
      parameter:inc_time_tmc            == inc_time_tmc
    )",
  filenames = "ppc_summary.rds",
  load_fun = readRDS
)

# for running remaining tasks
pars_df_remaining <- pars_df[which(is.na(fit_stats$dirs)), ]

# remove rows with no matches from pars_df (57/156 for VMMC countries NA!!)
if (any(is.na(fit_stats$dirs))) {
  pars_df <- pars_df[-which(is.na(fit_stats$dirs)), ]
}

fit_stats1 <- fit_stats$output

fit_stats_join <- bind_rows(lapply(fit_stats1, function(x) {
  as.data.frame(t(unlist(x)))
}))
names(fit_stats_join)[
  grepl("oos_obs", names(fit_stats_join))
] <- str_remove(
  names(fit_stats_join)[grepl("oos_obs", names(fit_stats_join))], 
  "oos_observations_within_PPD_"
)
names(fit_stats_join)[
  grepl("elpd.estimates1", names(fit_stats_join))
] <- str_remove(
  names(fit_stats_join)[
    grepl("elpd.estimates1", names(fit_stats_join))
  ], 
 "1" 
)
# names(fit_stats_join)[grepl("replacement_pars", names(fit_stats_join))] <- stringr::str_remove(
#   names(fit_stats_join)[grepl("replacement_pars", names(fit_stats_join))], "replacement_pars."
# )

fit_stats_join <- fit_stats_join %>% 
  select(-c(matches("elpd.pointwise"), matches("elpd.estimates"))) %>% 
  bind_cols(pars_df) %>% 
  pivot_longer(MMC.CI.0.5:MC.rmse) %>% 
  separate(col = name, into = c("type"), remove = FALSE, extra = "drop") %>% 
  mutate(name = str_remove_all(name, "MMC.|TMC.|MC.")) %>% 
  pivot_wider(names_from = name, values_from = value)

# find missing 
# pars_df %>% 
#   anti_join(
#     fit_stats_join, 
#     by = c("cntry", "rw_order", "inc_time_tmc", "paed_age_cutoff")
#   )


# best model for each country based on RMSE
fit_stats_join %>% 
  filter(rmse == min(rmse), .by = c(cntry, rw_order)) %>% 
  # filter(rw_order == 0) %>% 
  mutate(spec = paste0(
    "rw_order = ", 
    rw_order, 
    ", paed_age_cutoff = ", 
    paed_age_cutoff, 
    ", inc_time_tmc = ", 
    inc_time_tmc
  )) %>% 
  summarise(n(), .by = spec) %>% 
  filter(grepl("rw_order = 0", spec))

# Models pretty similar for VMMC countries, so essentially a modelling choice 
# Because TMC neads to vary for KEN, makes sense to give it a time TMC effect
# paediatric MMC cutoff agrees with VMMC policy, and does not significantly 
# effect fit (no survey data on coverage by age (current age, not circ age!)) 
# for under 15s anyway to assess fit for paediatric individuals, since surveys 
# do not ask under 15s
# -> Model used: time TMC, paediatric MMC age cutoff
