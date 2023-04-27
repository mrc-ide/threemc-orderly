#### Pull together hyperparameters and associated fit statistics ####

library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggtern)

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
iso3 <- iso3[!iso3 %in% c(vmmc_iso3, no_type_iso3)]

# only do complete countries for now (6/13)
# iso3 <- vmmc_iso3 <- c("KEN", "MOZ", "NAM", "RWA", "ZAF", "ZMB")
iso3 <- vmmc_iso3
iso3 <- iso3[!iso3 %in% c("LSO", "KEN")] # remaining countries, fitting now

# AR hyperpars (do a very course grid now, to begin with)
# ar_pars <- data.frame(
ar_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 0,
  "paed_age_cutoff"        = c(10, Inf),
  "inc_time_tmc"           = c(FALSE, TRUE),
  "logsigma_time_mmc"      = seq(0, 3, by = 1.5), 
  "logsigma_agetime_mmc"   = seq(0, 3, by = 1.5), 
  "logsigma_spacetime_mmc" = seq(-2, 1, by = 1.5)
)

# RW 2 hyperpars
# rw_1_pars <- data.frame(
rw_1_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 1,
  "paed_age_cutoff"        = c(10, Inf),
  "inc_time_tmc"           = c(FALSE, TRUE),
  "logsigma_time_mmc"      = seq(-1, 1, by = 1), 
  "logsigma_agetime_mmc"   = seq(-1, 1, by = 1), 
  "logsigma_spacetime_mmc" = seq(-1, 1, by = 1)
)

# RW 2 hyperpars
# rw_2_pars <- data.frame(
rw_2_pars <- tidyr::crossing(
  "cntry"                  = iso3,
  "rw_order"               = 2,
  "paed_age_cutoff"        = c(10, Inf),
  "inc_time_tmc"           = c(FALSE, TRUE),
  "logsigma_time_mmc"      = seq(-2, 1,  by = 1.5), 
  "logsigma_agetime_mmc"   = seq(-4, -1, by = 1.5), 
  "logsigma_spacetime_mmc" = seq(-6, -3, by = 1.5)
)

pars_df <- rbind(ar_pars, rw_1_pars, rw_2_pars)

pars_df <- pars_df %>% 
  filter(
    (cntry %in% vmmc_iso3 & paed_age_cutoff == 10 & inc_time_tmc == TRUE)  # |
      # (cntry %in% no_type_iso3 & paed_age_cutoff == Inf & inc_time_tmc == FALSE) |
      # (cntry %in% iso3 & paed_age_cutoff == Inf & inc_time_tmc == TRUE)
  )

# temp: remove unrun row for ZWE
pars_df <- pars_df %>% 
  filter(!(cntry == "ZWE" & rw_order == 1 & logsigma_time_mmc == 1 & logsigma_agetime_mmc == -1 & logsigma_spacetime_mmc == -1))

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
  task = "val_hyperpar_temporal_prior_investigation3", 
  parameters = pars_df,
  query = "latest(
      parameter:cntry                   == cntry && 
      parameter:rw_order                == rw_order &&
      parameter:paed_age_cutoff         == paed_age_cutoff &&
      parameter:inc_time_tmc            == inc_time_tmc && 
      parameter:logsigma_time_mmc      == logsigma_time_mmc &&
      parameter:logsigma_agetime_mmc   == logsigma_agetime_mmc && 
      parameter:logsigma_spacetime_mmc == logsigma_spacetime_mmc
    )",
  filenames = "ppc_summary.rds",
  load_fun = readRDS
)
# run instead if we already have directories!
# fit_stats <- load_orderly_data(
#   task = "val_hyperpar_temporal_prior_investigation3",
#   dirs = fit_stats$dirs,
#   filenames = "ppc_summary.rds",
#   load_fun = readRDS
# )

# remove rows with no matches from pars_df
if (any(is.na(fit_stats$dirs))) {
  pars_df <- pars_df[-which(is.na(fit_stats$dirs)), ]
}

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
readr::write_csv(fit_stats_join, "logsigma_hyperpars.csv")

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

fit_stats_join <- bind_rows(
  readr::read_csv("logsigma_hyperpars.csv"), 
  readr::read_csv("~/OneDrive/logsigma_hyperpars_extended.csv")) %>% 
  select(-...1)

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
    # fill = dark2
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

# violin plot showing similar information
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
    # fill = dark2
  )) + 
  geom_violin() + 
  facet_grid(name ~ rw_order) + 
  theme_bw() + 
  labs(x = "Parameter Value", y = "50% CI", fill = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = NA, colour = "white"), 
    panel.background = element_rect(fill = NA, color = "black"), 
    strip.text = element_text(size = 12)
  )

# bubble plot, averaging across countries
fit_stats_join_mean <- fit_stats_join %>% 
  select(-cntry) %>% 
  group_by(logsigma_time_mmc, logsigma_agetime_mmc, logsigma_spacetime_mmc, rw_order) %>% 
  summarise(across(everything(), mean))

fit_stats_join_mean_ar_1 <- fit_stats_join_mean %>% 
  filter(rw_order == 0)

plot_ly(
  data = fit_stats_join_mean_ar_1, 
  type = "scatter3d",
  x     = ~logsigma_time_mmc, 
  y     = ~logsigma_agetime_mmc, 
  z     = ~logsigma_spacetime_mmc, 
  size  = ~CI.0.5, 
  color = ~CI.0.5,
  marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 30) # sizes seem reasonable
)

# repeat for RW1, RW2

# ternary (contour?) plot
ggtern(
  data = fit_stats_join_mean_ar_1, 
  aes(x = logsigma_time_mmc,
      y = logsigma_agetime_mmc, 
      z = logsigma_spacetime_mmc)
) +
  # geom_point() + 
  theme_showarrows() + 
  geom_density_tern()

ggtern(
  data = fit_stats_join_mean_ar_1, 
  aes(x = logsigma_time_mmc,
      y = logsigma_agetime_mmc, 
      z = logsigma_spacetime_mmc)
) + 
  stat_density_tern(aes(fill = CI.0.5, alpha = CI.0.5, group = CI.0.5), geom = "polygon") + 
  theme_showarrows() + 
  geom_point() + 
  # geom_density_tern(aes(colour = CI.0.5)) +
  scale_colour_gradient2(low = "blue", mid = "green", high = "red") + 
  scale_fill_gradient2(low = "blue", mid = "green", high = "red") + 
  guides(alpha = "none")
  
  ggtern(data = fit_stats_join_mean_ar_1, 
         aes(x = logsigma_time_mmc,
             y = logsigma_agetime_mmc, 
             z = logsigma_spacetime_mmc, 
             group = CI.0.5)) + 
  stat_density_tern(geom = "polygon", n = 400,
                    aes(fill  = CI.0.5, alpha = CI.0.5)) +
  geom_point() +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red", name = "", breaks = 1:5, 
                      labels = c("low", "", "", "", "high"))  +
  scale_L_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_R_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  scale_T_continuous(breaks = 0:5 / 5, labels = 0:5/ 5) +
  labs(title = "Example Density/Contour Plot") +
  guides(fill = guide_colorbar(order = 1), alpha = guide_none()) +
  theme_rgbg() +
  # theme_noarrows() +
  theme(legend.justification = c(0, 1), 
        legend.position      = c(0, 1))
  
  # scale_T_continuous(limits = c(min(fit_stats_join_mean_ar_1$logsigma_spacetime_mmc),
  #                               max(fit_stats_join_mean_ar_1$logsigma_spacetime_mmc))) + 
  # scale_T_continuous(limits = c(0, 0.6)) + 
  # scale_L_continuous(limits = c(0.4, 1)) + 
  # scale_R_continuous(limits = c(-0.4, 0))
  NULL
  



# fit_stats_join %>% 
#   arrange(across(contains("logsigma"))) %>% 
#   mutate(
#     rw_order = case_when(
#       rw_order == 0 ~ "AR 1", 
#       rw_order == 1 ~ "RW 1", 
#       TRUE          ~ "RW 2"
#     ), 
#     line_lab = paste0(
#       "logsigma time MMC = ", logsigma_time_mmc, ", ",
#       "logsigma agetime MMC = ", logsigma_agetime_mmc # , ", ",
#       # "logsigma spacetime MMC = ", logsigma_spacetime_mmc
#     )
#   ) %>% 
#   pivot_longer(contains("logsigma"), values_to = "parameter_value") %>% 
#   ggplot(aes(
#     x = parameter_value,
#     y = CI.0.5, 
#     group = parameter_value,
#     # fill = as.factor(parameter_value),
#     colour = line_lab
#   )) + 
#   geom_line() + 
#   facet_grid(name ~ rw_order) + 
#   theme_bw() + 
#   labs(x = "Parameter Value", y = "50% CI", fill = "Parameter Value") + 
#   theme(
#     legend.position = "none",
#     strip.background = element_rect(fill = NA, colour = "white"), 
#     panel.background = element_rect(fill = NA, color = "black"), 
#     strip.text = element_text(size = 12)
#   )



# Also would be useful to have "interaction-style" plots
# TODO: Average across different countries!
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
  purrr::map(
    ~.x %>% 
      ggplot(aes(x = logsigma_time_mmc, y = CI.0.5, colour = types, group = types)) + 
      geom_line() + 
      ggtitle(.x$cntry[1]) + 
      facet_wrap(. ~ rw_order) + 
      guides(colour = "none") + 
      theme_bw() + 
      theme(
        legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "white"), 
        panel.background = element_rect(fill = NA, color = "black"), 
        strip.text = element_text(size = 12)
      )
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
