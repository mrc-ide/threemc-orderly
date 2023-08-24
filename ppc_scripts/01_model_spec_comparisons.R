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
# readr::write_csv(fit_stats_join, "01e_new_final_ppc_models_fit_stats.csv")

# For now: 
# Only look at countries where all model specifications have successfully fit
fit_stats_join <- fit_stats_join %>% 
  mutate(
    spec = paste0(
      "rw_order = ", 
      rw_order, 
      ", paed_age_cutoff = ", 
      paed_age_cutoff, 
      ", inc_time_tmc = ", 
      inc_time_tmc
    ), 
    vmmc = ifelse(cntry %in% vmmc_iso3, "VMMC", "Non-VMMC")
  ) %>% 
  relocate(spec:vmmc, .after = type)

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

#### Plots ####

# Idea: Want a plot showing average fit statistics for each model
# Similar to Adam's here https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pgph.0001731/1/pgph.0001731.s001.pdf?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20230811%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20230811T141640Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=393e80b57edcdf23eb5369e82673a4ff86fd9040dec463fc030a682ed57ccc3380410c119bac5db67882f42011c4a355dbfb463447c1715047caacb70872e0be36b53d6173e29042059e19643cdb52303ee7b8882189c48858e6aa71f1b27a263947fa88c56c3a573b11189570fb9e14c8a983f46d94f3e5c1b0b6369b717c7c88f1729c8ee66cbace7e3cedf7d3f77eac9affd101ff9f074102cab5d02b3f608cd1c72cbc8b6521773d6bbfd8a6aa5773e6bb5b2498fb5ef5a984f13fb8fb8f2f16d110955bbef0a278cad8d12673407c966287c0ebdb54fe952f526124a8ad0e1a4eea5e69fd90199eab08561f54c48c7c1172f1bada0a6d5bdd109b4ad1f9
# Could have value on y-axis, different model given line on x-axis, facets 
# for different fit stats (CRPS, RMSE, 95% Coverage) and different types
# Will be three different plots for three different RW orders
# Use colour scheme from survey plot
# Also need to add range as line in background! Just get Adam's code

# summarise fit stats across all countries
fit_stats_join_plt <- fit_stats_join %>% 
  group_by(type, vmmc, rw_order, paed_age_cutoff, inc_time_tmc) %>% 
  select(-c(mae, contains("ppd"))) %>% 
  summarise(
    crps = mean(crps, na.rm = TRUE),
    elpd = mean(elpd, na.rm = TRUE),
    rmse = mean(rmse, na.rm = TRUE),
    # across(elpd:ppd_0.950, ~ mean(., na.rm = TRUE)),
    # n = n(), 
    .groups = "drop"
  ) %>% 
  # label models
  mutate(
    mod_num = case_when(
      paed_age_cutoff == Inf & inc_time_tmc == TRUE ~
        1,
      paed_age_cutoff == 10 & inc_time_tmc == TRUE ~
        2,
      paed_age_cutoff == 10 & inc_time_tmc == FALSE ~
        3,
      paed_age_cutoff == Inf & inc_time_tmc == FALSE ~
        4
    ), 
    mod = case_when(
      paed_age_cutoff == Inf & inc_time_tmc == TRUE ~
        "Time TMC",
      paed_age_cutoff == Inf & inc_time_tmc == FALSE ~
        "Neither",
      paed_age_cutoff == 10 & inc_time_tmc == TRUE ~
        "Paed Cutoff, Time TMC",
      paed_age_cutoff == 10 & inc_time_tmc == FALSE ~
        "Paed Cutoff", 
      TRUE ~ "Unknown"
    ), 
    type = case_when(
      grepl("MMC", type) ~ "MMC",
      grepl("TMC", type) ~ "TMC",
      TRUE               ~ "MC"
    )
  ) %>% 
  relocate(mod, .after = inc_time_tmc)

fit_stats_join_plt_long <- fit_stats_join_plt %>% 
  # only do for AR 1 temporal prior & MC for now
  filter(type == "MC", rw_order == 0) %>%
  pivot_longer(elpd:n)
   
fit_stats_join_plt_long %>% 
  mutate(
    name = toupper(name),
    name = case_when(
      name == "ppd_0.500" ~ "50% CI Coverage", 
      name == "ppd_0.800" ~ "80% CI Coverage", 
      name == "ppd_0.950" ~ "95% CI Coverage", 
      TRUE                ~ name
    ) 
  ) %>% 
  filter(name %in% c("ELPD", "CRPS", "RMSE")) %>% 
  ggplot(aes(x = mod_num, y = value)) + 
  geom_point(aes(colour = factor(mod))) + 
  theme_bw() + 
  facet_wrap(vmmc ~ name, scales = "free") + 
  NULL



#### Analysis ####

# ignore circumcision type for now
fit_stats_join_partial <- fit_stats_join_partial %>% 
  filter(type != "MC coverage")

# Note: Only 18 of 33 countries fully available here!

# What do we need to do? 
# Determine the best model, for both VMMC and non-VMMC countries, in terms of: 
# RMSE, and CI coverage
# This will also be for each different type & rw_order
# Need best for each country
# Need average by type, random walk order and VMMC status
# Also need average by these for time TMC and paediatric age cutoff separately
# (so ignoring the other)

## best model by RMSE ##

# for non-VMMC
fit_stats_join_partial %>% 
  # filter(region == "WCA") %>% 
  filter(vmmc == "Non-VMMC") %>% 
  filter(rmse == min(rmse), .by = c(cntry, rw_order, type)) %>% 
  summarise(n = n(), .by = c(spec, type, rw_order)) %>% 
  arrange(rw_order, type, desc(n)) %>% 
  identity()

# for MC, best model is mixed, 
# However for MMC and TMC, it's nearly always the model with no 
# paed age cutoff and a time TMC effect
# Weird result for MC may be explained by MC being very high in many of these 
# countries anyway, so MC/TMC accuracy should be prioritised

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

## Best by CRPS ##

# for non-VMMC
fit_stats_join_partial %>% 
  # filter(region == "WCA") %>% 
  filter(!cntry %in% vmmc_iso3) %>% 
  filter(rmse == min(crps), .by = c(cntry, rw_order, type)) %>% 
  summarise(n = n(), .by = c(spec, type, rw_order)) %>% 
  arrange(rw_order, type, desc(n)) %>% 
  identity()

# for MC, best model is mixed, but for MMC and TMC, it's nearly always the
# model with no paed age cutoff and a time TMC effect
# Weird result for MC may be explained by MC being very high in many of these 
# countries anyway, so MC/TMC accuracy should be prioritised

# for VMMC
fit_stats_join_partial %>% 
  # filter(region == "ESA") %>% 
  filter(cntry %in% vmmc_iso3) %>% 
  filter(rmse == min(rmse), .by = c(cntry, rw_order, type)) %>% 
  summarise(n = n(), .by = c(spec, type, rw_order)) %>% 
  arrange(rw_order, type, desc(n)) %>% 
  identity()


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

