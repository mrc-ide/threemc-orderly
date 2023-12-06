#### Plots for Performing Model Calibration ####

#### Initial ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtern)
library(viridis)

#### Load Data ####

# load fit statistics for non-VMMC (or VMMC) countries
fit_stats <- readr::read_csv(
  "model_calibration_outputs/02b_fit_stats_pars_non_vmmc.csv.gz"
)

#### Calculations ####

# take mean fit statistics across all model hyperparameter/temporal prior specs
fit_stats_mean <- fit_stats %>% 
  group_by(rw_order, across(contains("logsigma"))) %>% 
  # may need to also add fit statistics here?
  summarise(across(contains("ppd"), ~ mean(.)), .groups = "drop")

# convert post coverages to how close they are calibrated to relative conf
fit_stats_mean_closest <- fit_stats_mean %>% 
  mutate(
    diff_0.500 = abs(ppd_0.500 - 0.5),
    diff_0.800 = abs(ppd_0.800 - 0.8),
    diff_0.950 = abs(ppd_0.950 - 0.95)
  ) %>% 
  select(-contains("ppd"))

#### Boxplots ####

# correlation between parameter values, CIs and fit stats
cors <- fit_stats %>% 
  group_by(rw_order) %>% 
  summarise(
    cor_ci_agetime_0.5    = cor(ppd_0.500,  logsigma_agetime_mmc),
    cor_ci_spacetime_0.5  = cor(ppd_0.500,  logsigma_spacetime_mmc),
    cor_ci_time_0.5       = cor(ppd_0.500,  logsigma_time_mmc),
    cor_ci_agetime_0.8    = cor(ppd_0.800,  logsigma_agetime_mmc),
    cor_ci_spacetime_0.8  = cor(ppd_0.800,  logsigma_spacetime_mmc),
    cor_ci_time_0.8       = cor(ppd_0.800,  logsigma_time_mmc),
    cor_ci_agetime_0.95   = cor(ppd_0.950, logsigma_agetime_mmc),
    cor_ci_spacetime_0.95 = cor(ppd_0.950, logsigma_spacetime_mmc),
    cor_ci_time_0.95      = cor(ppd_0.950, logsigma_time_mmc),
    cor_mae_agetime      = cor(mae, logsigma_agetime_mmc),
    cor_mae_spacetime    = cor(mae, logsigma_spacetime_mmc),
    cor_mae_time         = cor(mae, logsigma_time_mmc),
    cor_rmse_agetime      = cor(rmse, logsigma_agetime_mmc),
    cor_rmse_spacetime    = cor(rmse, logsigma_spacetime_mmc),
    cor_rmse_time         = cor(rmse, logsigma_time_mmc),
  )

# very low correlations between hyperparameter values and any fit stats
cors %>% 
  pivot_longer(!matches("rw_order")) %>% 
  # arrange(rw_order, name)
  arrange(desc(abs(value)))

# which CI sees biggest changes? 
cors %>% 
  pivot_longer(!matches("rw_order")) %>% 
  filter(!grepl("rmse", name)) %>% 
  group_split(rw_order) %>% 
  purrr::map(~ filter(., abs(value) == max(abs(value)))) %>% 
  bind_rows()


# boxplots for each rw_order and par value
fit_stats %>% 
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
    y = ppd_0.950, 
    group = parameter_value
  )) + 
  geom_boxplot() + 
  facet_grid(name ~ rw_order) + 
  theme_bw() + 
  labs(x = "Parameter Value", y = "95% CI", fill = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = NA, colour = "white"), 
    panel.background = element_rect(fill = NA, color = "black"), 
    strip.text = element_text(size = 12)
  )

fit_stats %>% 
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
    y = crps, 
    group = parameter_value # ,
    # fill = as.factor(parameter_value)
  ), fill = "blue") + 
  geom_boxplot() + 
  facet_wrap(name ~ rw_order, scales = "free") + 
  theme_bw() + 
  labs(x = "Parameter Value", y = "95% CI", fill = "Parameter Value") + 
  theme(
    strip.background = element_rect(fill = NA, colour = "white"), 
    panel.background = element_rect(fill = NA, color = "black"), 
    strip.text = element_text(size = 12)
  )

# Also would be useful to have "interaction-style" plots (useless!!)
# plots_time <- fit_stats %>% 
#   mutate(
#     types = paste0(
#       # "cntry = ", 
#       # cntry,
#       "logsigma_agetime_mmc = ", 
#       logsigma_agetime_mmc, 
#       ", logsigma_spacetime_mmc = ",
#       logsigma_spacetime_mmc
#     )
#   ) %>% 
#   # filter(logsigma_agetime_mmc == 0, logsigma_spacetime_mmc == -2) %>% 
#   group_split(cntry) %>% 
#   purrr::map(~.x %>% 
#     ggplot(aes(x = logsigma_time_mmc, y = ppd_0.500, colour = types, group = types)) + 
#     geom_line() + 
#     facet_wrap(. ~ rw_order) + 
#     guides(colour = "none")
#   )
# 
# plots_agetime <- fit_stats %>% 
#   mutate(
#     types = paste0(
#       "logsigma_time_mmc = ", 
#       logsigma_time_mmc, 
#       ", logsigma_spacetime_mmc = ",
#       logsigma_spacetime_mmc
#     )
#   ) %>% 
#   # filter(logsigma_time_mmc == 0, logsigma_spacetime_mmc == -2) %>% 
#   group_split(cntry) %>% 
#   purrr::map(~.x %>% 
#                ggplot(aes(
#                  x = logsigma_agetime_mmc, 
#                  y = ppd_0.500, 
#                  colour = types, 
#                  group = types
#                )) + 
#                geom_line() + 
#                facet_wrap(. ~ rw_order) + 
#                guides(colour = "none")
#    )
# 
# plots_spacetime <- fit_stats %>% 
#   mutate(
#     types = paste0(
#       "logsigma_agetime_mmc = ", 
#       logsigma_agetime_mmc, 
#       ", logsigma_time_mmc = ",
#       logsigma_time_mmc
#     )
#   ) %>% 
#   # filter(logsigma_agetime_mmc == 0, logsigma_spacetime_mmc == -2) %>% 
#   group_split(cntry) %>% 
#   purrr::map(~.x %>% 
#                ggplot(aes(x = logsigma_spacetime_mmc, y = ppd_0.500, colour = types, group = types)) + 
#                geom_line() + 
#                facet_wrap(. ~ rw_order) + 
#                guides(colour = "none")
#   )

#### Ternary Plot ####

# Temp: force all logsigma values to be positive
fit_stats_tern <- fit_stats_mean_closest %>% 
  group_by(rw_order) %>% 
  mutate(
    across(contains("logsigma"), ~ case_when(
      min(.) < 0 ~ . + abs(min(.)),
      TRUE       ~ .
    ))
  ) %>% 
  ungroup()

# function to normalise hyperparameter values between 0 and 1
normalise <- function(x) (x - min(x)) / (max(x) - min(x))

# normalise hyperparameter values to be between 0 and 1
fit_stats_tern_norm <- fit_stats_tern %>% 
  group_by(rw_order) %>% 
  mutate(across(contains("logsigma"), ~ normalise(.))) %>% 
  ungroup()
 
fit_stats_tern_norm %>% 
  ggtern(
    aes(
      logsigma_time_mmc, 
      logsigma_agetime_mmc, 
      logsigma_spacetime_mmc, 
      value = diff_0.950
    )
  ) +
  stat_interpolate_tern(
    geom = "polygon",
    formula = value ~ x + y,
    method = lm,
    n = 100,
    breaks = seq(0, 1000, by = 100),
    aes(fill=..level..),
    expand = 1
  ) +
  geom_hex_tern(
    # binwidth = 0.01, size = 0.1, color = "white", show.legend = FALSE
    stat = "hex_tern",
    fun = "mean",
    na.rm = TRUE, 
    binwidth = 0.1
  ) + 
  geom_point() + 
  facet_wrap(~ rw_order) + 
  scale_fill_viridis() +
  theme_rgbw() +
  # theme_gridsontop()
  NULL
# therefore, 95% coverage is best for higher logsigma_agetime, 
# higher logsigma_spacetime_mmc, and lower logsigma_time_mmc!
# TODO: Find out why
