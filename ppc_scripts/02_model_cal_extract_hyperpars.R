#### Pull together hyperparameters and associated fit statistics ####

library(tidyr)
library(dplyr)
library(ggplot2)
library(parallel)
source("paper_poster_plots/paper/scripts/00_funs.R")


#### Parameter Values ####

# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" 
)
# countries with no type info
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")
# countries with only one survey, cannot perform OOS analysis on these
single_year_iso3 <- c(
  "AGO", "CAF", "COD", "GAB", "GIN", "GMB", "NER", "SEN", "TGO", "BFA"
)
iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
          "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
          "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
          "ZAF", "LBR", "GHA", "GMB", "NGA", "COD")
iso3 <- iso3[!iso3 %in% c(no_type_iso3, single_year_iso3)]

# only do VMMC countries
iso3 <- vmmc_iso3
# only do non-VMMC countries
# iso3 <- iso3[!iso3 %in% vmmc_iso3]

# AR hyperpars (do a very course grid now, to begin with)
# ar_pars <- data.frame(
ar_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 0,
  "paed_age_cutoff"        = c(10, Inf),
  "inc_time_tmc"           = c(FALSE, TRUE),
  "logsigma_time_mmc"      = seq(-1.5, 3, by = 1.5), 
  "logsigma_agetime_mmc"   = seq(0, 4.5, by = 1.5), 
  "logsigma_spacetime_mmc" = seq(-3.5, 1, by = 1.5)
)

# RW 2 hyperpars
# rw_1_pars <- data.frame(
rw_1_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 1,
  "paed_age_cutoff"        = c(10, Inf),
  "inc_time_tmc"           = c(FALSE, TRUE),
  "logsigma_time_mmc"      = seq(-1, 2, by = 1), 
  "logsigma_agetime_mmc"   = seq(-1, 2, by = 1), 
  "logsigma_spacetime_mmc" = seq(-2, 1, by = 1)
)

# RW 2 hyperpars
# rw_2_pars <- data.frame(
rw_2_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 2,
  "paed_age_cutoff"        = c(10, Inf),
  "inc_time_tmc"           = c(FALSE, TRUE),
  "logsigma_time_mmc"      = seq(-2, 2.5,  by = 1.5), 
  "logsigma_agetime_mmc"   = seq(-4, 0.5, by = 1.5), 
  "logsigma_spacetime_mmc" = seq(-6, -1.5, by = 1.5)
)

pars_df <- rbind(ar_pars, rw_1_pars, rw_2_pars)

pars_df <- pars_df %>%
  filter(
    (cntry %in% vmmc_iso3 & paed_age_cutoff == 10 & inc_time_tmc == TRUE) |
      # (cntry %in% no_type_iso3 & paed_age_cutoff == Inf & inc_time_tmc == FALSE) |
      (cntry %in% iso3 & paed_age_cutoff == Inf & inc_time_tmc == TRUE)
  )

# for now, don't look at GHA
pars_df <- pars_df %>% 
  filter(cntry != "GHA")

# pars_df$paed_age_cutoff <- ifelse(
#   pars_df$cntry %in% vmmc_iso3, 
#   10, 
#   Inf
# )
# pars_df$inc_time_tmc <- TRUE

# temp: remove unrun row for ZWE
# pars_df <- pars_df %>% 
#   filter(!(cntry == "ZWE" & rw_order == 1 & logsigma_time_mmc == 1 & logsigma_agetime_mmc == -1 & logsigma_spacetime_mmc == -1))

# load parameter values
pars_df <- readr::read_csv(
  "model_calibration_outputs/01_pars_df_vmmc_inc_ids.csv"
  # "model_calibration_outputs/02_pars_df_non_vmmc_inc_ids.csv"
) %>% 
  # search for remaining unrun tasks ids
  # filter(is.na(ids)) %>% 
  # select(-ids) %>% 
  identity() 


#### load orderly data ####

# search for orderly task corresponding to each line in pars_df
if (!"ids" %in% names(pars_df)) {
  fit_dirs <- load_orderly_data(
    task = "val_hyperpar_temporal_prior_investigation3", 
    parameters = pars_df,
    query = "latest(
        parameter:cntry                  == cntry && 
        parameter:rw_order               == rw_order &&
        parameter:paed_age_cutoff        == paed_age_cutoff &&
        parameter:inc_time_tmc           == inc_time_tmc && 
        parameter:logsigma_time_mmc      == logsigma_time_mmc &&
        parameter:logsigma_agetime_mmc   == logsigma_agetime_mmc && 
        parameter:logsigma_spacetime_mmc == logsigma_spacetime_mmc
      )"
  )$dirs
} else {
  fit_dirs <- pars_df$ids
}

# add id of tasks to corresponding parameters 
# investigate & rerun on the cluster!
pars_df$ids <- fit_dirs

# save for later
# readr::write_csv(
#    pars_df, 
#   "model_calibration_outputs/01_pars_df_vmmc_inc_ids.csv"
#   # "model_calibration_outputs/02_pars_df_non_vmmc_inc_ids_gha.csv"
#   # "model_calibration_outputs/02_pars_df_non_vmmc_inc_ids.csv"
# )

# tabulate NAs for each model specification
# VMMC: 363 / 2496 are NAs for 19/39 model combos (now only 14! Good enough)
# Non-VMMC: 820 / 1920 are NAs for ?/? model combos
pars_df %>% 
  filter(is.na(ids)) %>% 
  group_by(cntry, rw_order, paed_age_cutoff, inc_time_tmc) %>% 
  summarise(n = n()) %>% 
  ungroup() 


# load fit statistics from each of these directories
# TODO: Investigate where ppc_summary.csv is blank for GHA!
# fit_stats <- load_orderly_data(
#   task = "val_hyperpar_temporal_prior_investigation3", 
#   dirs = fit_dirs[!is.na(fit_dirs)],
#   filenames = "ppc_summary.csv"
# )$output
# 
# # remove blank fits (should only be required for GHA)
# fit_stats <- fit_stats[vapply(fit_stats, nrow, numeric(1)) != 0]
# 
# fit_stats <- bind_rows(fit_stats)
# gc()

fit_stats <- readr::read_csv(
  "model_calibration_outputs/01a_fit_stats_vmmc.csv.gz"
)

# save 
# readr::write_csv(
#   fit_stats,
#   "model_calibration_outputs/01a_fit_stats_vmmc.csv.gz"
#   # "model_calibration_outputs/02a_fit_stats_non_vmmc.csv.gz"
# )

# remove rows with no matches from pars_df
if (any(is.na(fit_dirs))) {
  pars_df <- pars_df[-which(is.na(fit_dirs)), ]
}

# join parameter values to fit statistics
fit_stats_join <- bind_rows(lapply(group_split(fit_stats, type), function(x) {
  bind_cols(pars_df, x)
}))


## no longer needed; fit statistics are saved in 
fit_stats1 <- fit_stats$output

fit_stats_join <- bind_rows(lapply(fit_stats1, function(x) {
    as.data.frame(t(unlist(x)))
}))

names(fit_stats_join)[grepl("oos_obs", names(fit_stats_join))] <- stringr::str_remove(
  names(fit_stats_join)[grepl("oos_obs", names(fit_stats_join))], "oos_observations_within_PPD_"
)
names(fit_stats_join)[names(fit_stats_join) == "elpd.estimates1"] <- "elpd"
names(fit_stats_join)[grepl("replacement_pars", names(fit_stats_join))] <- stringr::str_remove(
  names(fit_stats_join)[grepl("replacement_pars", names(fit_stats_join))], "replacement_pars."
)

fit_stats_join <- fit_stats_join %>% 
  select(-c(matches("elpd.pointwise"), matches("elpd.estimates"))) %>% 
  bind_cols(select(pars_df, cntry, rw_order))

# save
# readr::write_csv(fit_stats_join, "logsigma_hyperpars.csv")
readr::write_csv(fit_stats_join, "logsigma_hyperpars_1.csv")


#### Find best hyperparameters ####

# in terms of CI
fit_stats_join %>% 
  group_by(
    cntry, 
    # rw_order, 
    # logsigma_time_mmc, 
    # logsigma_agetime_mmc, 
    # logsigma_spacetime_mmc
  ) %>% 
  arrange(desc(CI.0.95)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # arrange(cntry, desc(CI.0.95)) %>% 
  identity()

# very different results here for each country!
#  1 RW2 model preferred, 1 RW1, 3 AR1

# For AR1, hyperparameters are very mixed, could be due to low number of countries
# For RW1, same, but lower logsigma_spacetime_mmc seems to be preferred
# for RW2, larger logsigma_time_mmc & logsigma_spacetime_mmc preferred, while 
# lower logsigma_agetime_mmc seems to do better

# Hoping things might become a little bit clearer with more countries to look 
# at!

# Perhaps I also need to evaluate my starting hyperparameter grid search! Are 
# they realistic values for 

# in terms of rmse (almost all prefer AR1 model with lower variance hyperpars!)
fit_stats_join %>% 
  group_by(cntry) %>% 
  arrange(rmse) %>% 
  slice(1) %>% 
  ungroup()
  
# for each statistic, something that summarises how that varies across the 
# different hyperparameter values
# Read 2017 EPP paper for an example

# Would like to have a best model averaged over all countries
# Best model over most countries
# Does that best model look really bad in some settings?

#### Exploration ####

# correlation between parameter values, CIs and fit stats
cors <- fit_stats_join %>% 
  group_by(rw_order) %>% 
  summarise(
    cor_ci_agetime_0.5    = cor(CI.0.5,  logsigma_agetime_mmc),
    cor_ci_spacetime_0.5  = cor(CI.0.5,  logsigma_spacetime_mmc),
    cor_ci_time_0.5       = cor(CI.0.5,  logsigma_time_mmc),
    cor_ci_agetime_0.8    = cor(CI.0.8,  logsigma_agetime_mmc),
    cor_ci_spacetime_0.8  = cor(CI.0.8,  logsigma_spacetime_mmc),
    cor_ci_time_0.8       = cor(CI.0.8,  logsigma_time_mmc),
    cor_ci_agetime_0.95   = cor(CI.0.95, logsigma_agetime_mmc),
    cor_ci_spacetime_0.95 = cor(CI.0.95, logsigma_spacetime_mmc),
    cor_ci_time_0.95      = cor(CI.0.95, logsigma_time_mmc),
    cor_rmse_agetime      = cor(rmse, logsigma_agetime_mmc),
    cor_rmse_spacetime    = cor(rmse, logsigma_spacetime_mmc),
    cor_rmse_time         = cor(rmse, logsigma_time_mmc)
  )

cors %>% 
  select(-c(contains("0.8"), contains("0.95")))

cors %>% 
  pivot_longer(!matches("rw_order")) %>% 
  arrange(rw_order, name) %>% 
  identity()

# which CI sees biggest changes? 
cors %>% filter(!grepl("rmse", name)) %>% 
  group_split(rw_order) %>% 
  purrr::map(~ filter(., abs(value) == max(abs(value)))) %>% 
  bind_rows()
# each time it's the 50th% percentile CI which changes the most, therefore plot with this!


# Notes: 
#  

# boxplots for each rw_order and par value
fit_stats_join %>% 
  arrange(across(contains("logsigma"))) %>% 
  mutate(
    rw_order = case_when(
      rw_order == 0 ~ "AR 1", 
      rw_order == 1 ~ "RW 1", 
      TRUE          ~ "RW 2"
    )
  ) %>% 
  pivot_longer(contains("logsigma"), values_to = "parameter_value") %>% 
  ggplot(aes(
    x = parameter_value,
    y = CI.0.5, 
    group = parameter_value,
    fill = as.factor(parameter_value)
  )) + 
  geom_boxplot() + 
  facet_grid(name ~ rw_order) + 
  theme_bw() + 
  labs(x = "Parameter Value", y = "50% CI", fill = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = NA, colour = "white"), 
    panel.background = element_rect(fill = NA, color = "black"), 
    strip.text = element_text(size = 12)
  )

# Also would be useful to have "interaction-style" plots
plots_time <- fit_stats_join %>% 
  mutate(
    types = paste0(
      # "cntry = ", 
      # cntry,
      "logsigma_agetime_mmc = ", 
      logsigma_agetime_mmc, 
      ", logsigma_spacetime_mmc = ",
      logsigma_spacetime_mmc
    )
  ) %>% 
  # filter(logsigma_agetime_mmc == 0, logsigma_spacetime_mmc == -2) %>% 
  group_split(cntry) %>% 
  purrr::map(~.x %>% 
    ggplot(aes(x = logsigma_time_mmc, y = CI.0.5, colour = types, group = types)) + 
    geom_line() + 
    facet_wrap(. ~ rw_order) + 
    guides(colour = "none")
  )

plots_agetime <- fit_stats_join %>% 
  mutate(
    types = paste0(
      "logsigma_time_mmc = ", 
      logsigma_time_mmc, 
      ", logsigma_spacetime_mmc = ",
      logsigma_spacetime_mmc
    )
  ) %>% 
  # filter(logsigma_time_mmc == 0, logsigma_spacetime_mmc == -2) %>% 
  group_split(cntry) %>% 
  purrr::map(~.x %>% 
               ggplot(aes(
                 x = logsigma_agetime_mmc, 
                 y = CI.0.5, 
                 colour = types, 
                 group = types
               )) + 
               geom_line() + 
               facet_wrap(. ~ rw_order) + 
               guides(colour = "none")
   )

plots_spacetime <- fit_stats_join %>% 
  mutate(
    types = paste0(
      "logsigma_agetime_mmc = ", 
      logsigma_agetime_mmc, 
      ", logsigma_time_mmc = ",
      logsigma_time_mmc
    )
  ) %>% 
  # filter(logsigma_agetime_mmc == 0, logsigma_spacetime_mmc == -2) %>% 
  group_split(cntry) %>% 
  purrr::map(~.x %>% 
               ggplot(aes(x = logsigma_spacetime_mmc, y = CI.0.5, colour = types, group = types)) + 
               geom_line() + 
               facet_wrap(. ~ rw_order) + 
               guides(colour = "none")
  )
