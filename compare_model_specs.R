#### Pull together fit statistics for different model specs ####

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
source("paper_poster_plots/paper/scripts/00_funs.R")

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
# pars_df <- pars_df %>%
#   filter(
#       rw_order == 0,
#       (cntry %in% no_type_iso3 & paed_age_cutoff == Inf & inc_time_tmc == FALSE) |
#       (cntry %in% vmmc_iso3 & paed_age_cutoff == 10 & inc_time_tmc == TRUE) |
#       (cntry %in% iso3 & paed_age_cutoff == Inf & inc_time_tmc == TRUE)
#  )

# pars_df <- pars_df %>% 
#   filter(cntry == "ZAF")


#### load orderly data ####

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

# add VMMC/non-VMMC classification
fit_stats_join <- fit_stats_join %>% 
  mutate(vmmc = ifelse(cntry %in% vmmc_iso3, "VMMC", "non-VMMC")) %>% 
  relocate(vmmc, .after = type)

# save 
# readr::write_csv(fit_stats_join, "./fit_stats_compare_specs.csv")

fit_stats_join <- fit_stats_join %>% 
  mutate(spec = paste0(
    "rw_order = ", 
    rw_order, 
    ", paed_age_cutoff = ", 
    paed_age_cutoff, 
    ", inc_time_tmc = ", 
    inc_time_tmc
  )) 

# best model for TMC for each country based on RMSE
fit_stats_join %>% 
  filter(rmse == min(rmse), .by = c(cntry, type, vmmc, rw_order, paed_age_cutoff, inc_time_tmc)) %>% 
  # summarise(n(), .by = spec) %>% 
  summarise(n = n(), .by = c(spec, type, vmmc)) %>% 
  arrange(vmmc, type, desc(n)) %>% 
  filter(grepl("rw_order = 0", spec)) %>% 
  # readr::write_csv("best_mods_ar_1_rmse.csv")
  identity()

# why is non-VMMC MC missing here?!?

# best model based on 0.95 CI
fit_stats_join %>% 
  filter(CI.0.95 == max(CI.0.95), .by = c(cntry, type, vmmc, rw_order)) %>% 
  summarise(n = n(), .by = c(spec, type, vmmc)) %>% 
  arrange(vmmc, type, desc(n)) %>% 
  filter(grepl("rw_order = 0", spec)) %>% 
  readr::write_csv("best_mods_ar_1_ci_0.95.csv")


# Models pretty similar for VMMC countries, so essentially a modelling choice 
# Because TMC neads to vary for KEN, makes sense to give it a time TMC effect
# paediatric MMC cutoff agrees with VMMC policy, and does not significantly 
# effect fit (no survey data on coverage by age (current age, not circ age!)) 
# for under 15s anyway to assess fit for paediatric individuals, since surveys 
# do not ask under 15s
# -> Model used: time TMC, paediatric MMC age cutoff
