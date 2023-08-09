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
iso3 <- iso3[!iso3 %in% no_type_iso3]
# iso3 <- iso3[!iso3 %in% c(vmmc_iso3, no_type_iso3)]

# test with VMMC countries
# iso3 <- vmmc_iso3

# test with specific countries
# iso3 <- c("AGO", "MLI", "SWZ")
# iso3 <- "BDI"

pars_df <- tidyr::crossing(
  "cntry" = iso3,
  # cntry = c(iso3, vmmc_iso3, no_type_iso3),
  "rw_order" = c(0, 1, 2),
  "paed_age_cutoff" = c(10, Inf),
  "inc_time_tmc" = c(FALSE, TRUE)
)

# temporary test
# pars_df <- pars_df[1:30, ]

# uncomment to only look at models we are currently using
# pars_df <- pars_df %>%
#   filter(
#       rw_order == 0,
#       (cntry %in% no_type_iso3 & paed_age_cutoff == Inf & inc_time_tmc == FALSE) |
#       (cntry %in% vmmc_iso3 & paed_age_cutoff == 10 & inc_time_tmc == TRUE) |
#       (cntry %in% iso3 & paed_age_cutoff == Inf & inc_time_tmc == TRUE)
#  )

#### Load and Preprocess ####

fit_stats <- load_orderly_data(
  task = "01e_new_final_ppc_models", 
  parameters = pars_df,
  query = "latest(
      parameter:cntry                   == cntry && 
      parameter:rw_order                == rw_order &&
      parameter:paed_age_cutoff         == paed_age_cutoff &&
      parameter:inc_time_tmc            == inc_time_tmc
    )",
  filenames = "ppc_summary.csv"
)

# for running remaining tasks
pars_df_remaining <- pars_df[which(is.na(fit_stats$dirs)), ]

# remove rows with no matches from pars_df (57/156 for VMMC countries NA!!)
if (any(is.na(fit_stats$dirs))) {
  pars_df <- pars_df[-which(is.na(fit_stats$dirs)), ]
}

fit_stats1 <- fit_stats$output

fit_stats1 <- bind_rows(lapply(seq_along(fit_stats1), function(i) {
  if (nrow(fit_stats1[[i]]) > 0) {
    bind_cols(pars_df[i, ], fit_stats1[[i]])
  } else return(data.frame())
}))

# find missing 
# pars_df %>% 
#   anti_join(
#     fit_stats_join, 
#     by = c("cntry", "rw_order", "inc_time_tmc", "paed_age_cutoff")
#   )

fit_stats_join <- fit_stats1 %>% 
  left_join(
    threemc::esa_wca_regions, 
    by = c("cntry" = "iso3")
  ) %>% 
  relocate(c(region, four_region), .after = "cntry")
# fit_stats_join <- readr::read_csv("01e_new_final_ppc_models_fit_stats.csv")

# For now: 
# Only look at countries where all model specifications have successfully fit
fit_stats_join <- fit_stats_join %>% 
  mutate(spec = paste0(
    "rw_order = ", 
    rw_order, 
    ", paed_age_cutoff = ", 
    paed_age_cutoff, 
    ", inc_time_tmc = ", 
    inc_time_tmc
  ))

# countries with all model specs ran 
iso3_full <- fit_stats_join %>% 
  count(cntry) %>% 
  arrange(n) %>% 
  filter(n == max(n)) %>% 
  pull(cntry)
print(length(iso3_full))
print(length(unique(fit_stats_join$cntry)))

fit_stats_join_partial <- fit_stats_join %>% 
  filter(cntry %in% iso3_full)

# readr::write_csv(fit_stats_join, "01e_new_final_ppc_models_fit_stats.csv")
# readr::write_csv(
#   fit_stats_join_partial, 
#   "01e_new_final_ppc_models_fit_stats_full.csv"
# )

#### Analysis ####

# What do we need to do? 
# Determine the best model, for both VMMC and non-VMMC countries, in terms of: 
# RMSE, and CI coverage
# This will also be for each different type & rw_order

## best model by RMSE ##

# for VMMC
fit_stats_join_partial %>% 
  # filter(region == "ESA") %>% 
  filter(cntry %in% vmmc_iso3) %>% 
  filter(rmse == min(rmse), .by = c(cntry, rw_order, type)) %>% 
  summarise(n = n(), .by = c(spec, type, rw_order)) %>% 
  arrange(rw_order, type, desc(n)) %>% 
  identity()

# For VMMC countries, seems to be little difference between the models, which 
# is unsurprising. However, having a paediatric age cutoff of 10 seems to be 
# preferred, which was again to be expected. 

# for non-VMMC
fit_stats_join_partial %>% 
  # filter(region == "WCA") %>% 
  filter(!cntry %in% vmmc_iso3) %>% 
  filter(rmse == min(rmse), .by = c(cntry, rw_order, type)) %>% 
  summarise(n = n(), .by = c(spec, type, rw_order)) %>% 
  arrange(rw_order, type, desc(n)) %>% 
  identity()

# for MC, best model is mixed, but for MMC and TMC, it's nearly always the
# model with no paed age cutoff and a time TMC effect
# Weird result for MC may be explained by MC being very high in many of these 
# countries anyway, so MC/TMC accuracy should be prioritised


## best by coverage ##

# ESA
fit_stats_join_partial %>% 
  filter(region == "ESA") %>% 
  filter(ppd_0.950 == max(ppd_0.950), .by = c(cntry, rw_order, type)) %>% 
  summarise(n = n(), .by = c(spec, type, rw_order)) %>% 
  arrange(rw_order, type, desc(n)) %>% 
  identity()

# WCA
fit_stats_join_partial %>% 
  filter(region == "WCA") %>% 
  filter(ppd_0.950 == max(ppd_0.950), .by = c(cntry, rw_order, type)) %>% 
  summarise(n = n(), .by = c(spec, type, rw_order)) %>% 
  arrange(rw_order, type, desc(n)) %>% 
  identity()
 
# Notes:  
# for ESA, CI coverage is best for model with paed_age_cutoff = 10, 
# inc_time_tmc = TRUE for almost all types and rw_order combinations. 
# Model choice is, unsurprisingly, less clear than for WCA, as there is not such
# a drastic difference between the fits for each model spec

# For WCA, it is very pleasing to see that the model with paed_age_cutoff = Inf 
# and inc_time_tmc = TRUE is preferred for every type and rw_order. This is 
# unsurprising, as these parameters have quite a large effect on model fit for 
# WCA countries. 

