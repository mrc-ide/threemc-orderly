#### Table of fit statistics for each country ####

# Need table of average fit statistics for each model specification, rw_order, 
# VMMC vs non-VMMC
# use Matt's previous table as TeX template
# Bold best fit stats for each rw_order and VMMC status

# Would also like same table, but with number of times each model is
# selected as the best for each country

# initial
fit_stats_join1 <- fit_stats_join %>% 
  select(-c(mae, contains("ppd"))) %>% 
  mutate(
    mod = case_when(
      paed_age_cutoff == Inf & inc_time_tmc == TRUE ~
        "Time TMC",
      paed_age_cutoff == Inf & inc_time_tmc == FALSE ~
        "Neither",
      paed_age_cutoff == 10 & inc_time_tmc == TRUE ~
        # "Paed Cutoff, Time TMC",
        "Both",
      paed_age_cutoff == 10 & inc_time_tmc == FALSE ~
        "Paed Cutoff", 
      TRUE ~ "Unknown"
    ), 
    mod = factor(
      mod, levels = c("Neither", "Time TMC", "Paed Cutoff", "Both")
    )
  )


#### Table 1: Average fit statistics for each model specification ####

fit_stats_mean <- fit_stats_join1 %>%
  # summarise, removing country
  group_by(type, vmmc, rw_order, mod) %>% 
  summarise(
    across(matches(c("crps", "elpd", "rmse")), ~ mean(., na.rm = TRUE)),
    .groups = "drop"
  ) %>% 
  rename("elpd_mean" = "elpd", "crps_mean" = "crps", "rmse_mean" = "rmse") %>% 
  pivot_longer(
    cols = starts_with(c("elpd", "crps", "rmse"))
  ) %>% 
  separate(
    name, 
    into = c("metric", "stat"), 
    extra = "merge", 
    fill = "left"
  ) %>%
  pivot_wider(
    names_from  = "stat",
    values_from = "value"
  ) %>%
  group_split(metric, rw_order, type, vmmc) %>%
  lapply(function(x)
    x %>%
      mutate(
        min_idx = (mean == min(mean, na.rm = TRUE)),
        max_idx = (mean == max(mean, na.rm = TRUE)),
        best_idx = ifelse(metric %in% c("rmse", "crps"), min_idx, max_idx)
      )
  ) %>%
  bind_rows() %>% 
  # Change label text
  mutate(
    metric = toupper(metric), 
    type = stringr::str_remove(type, " coverage"), 
    best_mod = ifelse(best_idx, as.character(mod), NA)
  ) %>% 
  select(vmmc, rw_order, type, metric, mod, mean, best_mod) %>% 
  group_by(vmmc, rw_order, type, metric) %>% 
  tidyr::fill(best_mod, .direction = "updown") %>% 
  ungroup() %>% 
  pivot_wider(names_from = mod, values_from = mean) %>% 
  relocate(best_mod, .after = everything()) %>% 
  arrange(vmmc, rw_order, type, metric)

fit_stats_mean %>% 
  janitor::tabyl(best_mod, rw_order, vmmc) %>% 
  janitor::adorn_totals("col") %>% 
  # `[[`(1) %>% 
  # View() %>% 
  identity()
  
# can also tabulate how many times each model is selected as "best", by having 
# the best average value for each metric
table1b <- tidyr::crossing(
  distinct(fit_stats_mean, vmmc, rw_order, type, metric), 
  "best_mod" = unique(as.character(fit_stats_join1$mod))
) %>% 
  left_join(
    fit_stats_mean %>% 
      group_by(vmmc, rw_order, type, metric) %>% 
      count(best_mod)
  ) %>% 
  group_by(vmmc, rw_order, type, best_mod) %>% 
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
  arrange(vmmc, rw_order, type) %>% 
  tidyr::pivot_wider(names_from = best_mod, values_from = n)
  


# Notes: 
# MMC & TMC more important for non-VMMC, since MC is so high for all models. 
# non-VMMC, rw_order 0: 
# 

# non-VMMC, rw_order 1: 

# non-VMMC, rw_order 2:

# VMMC, rw_order 0: 

# VMMC, rw_order 1: 

# VMMC, rw_order 2:


#### Table 2: N times model deemed best for each model specification ####

# for each model spec (and other grouping variables), find the # times 
# it was deemed the best

# pivot fit statistics into one "metric" column
fit_stats_metrics <- fit_stats_join1 %>% 
  select(cntry, vmmc, type, rw_order, vmmc, mod, elpd, crps, rmse) %>% 
  tidyr::pivot_longer(c("elpd", "crps", "rmse"), names_to = "metric") %>% 
  arrange(cntry, vmmc, type, rw_order, metric, mod)

# find best for each set of grouping variables, notably ignoring country
fit_stats_best <- fit_stats_metrics %>% 
  group_by(cntry, vmmc, type, rw_order, metric) %>% 
  filter(
    metric %in% c("rmse", "crps") & value == min(value) | 
      metric %in% c("elpd") & value == max(value)
  ) %>% 
  ungroup() %>% 
  count(vmmc, rw_order, type, metric, mod, .drop = FALSE, name = "n_best") %>% 
  arrange(mod) %>% 
  # Change label text
  mutate(
    metric = toupper(metric), 
    type = stringr::str_remove(type, " coverage")
  ) %>% 
  pivot_wider(names_from = mod, values_from = n_best) %>% 
  arrange(vmmc, rw_order, type, metric)

# can also ignore fit statistics
fit_stats_best_totals <- fit_stats_best %>% 
  group_by(vmmc, rw_order, type) %>% 
  summarise(across(Neither:Both, ~ sum(., na.rm = TRUE)), .groups = "drop") %>% 
  mutate(metric = "Total")

# function to bold text
bold_char <- function(char) {
  paste0("\\bf{", char, "}")
}

## Exported table to TeX
table2 <- bind_rows(fit_stats_best, fit_stats_best_totals) %>% 
  arrange(vmmc, rw_order, type, metric) %>% 
  # Need to highlight best model in bold (also for totals across metrics)
  pivot_longer(Neither:Both) %>% 
  group_by(across(vmmc:metric)) %>% 
  mutate(is_best = (value == max(value, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(
    value  = as.character(value),
    # might want to use (2?) horizontal line(s) for Total, rather than bold
    value  = case_when(
      # is_best | metric == "Total" ~ bold_char(value),
      is_best ~ bold_char(value),
      TRUE    ~ value
    ) # , 
    # type   = ifelse(metric == "Total", bold_char(type), type),
    # metric = ifelse(metric == "Total", bold_char(metric), metric)
  ) %>% 
  select(-is_best) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  # Change rw_order to be more informative
  mutate(rw_order = case_when(
    rw_order == 0 ~ "AR1", 
    rw_order == 1 ~ "RW1", 
    TRUE          ~ "RW2"
  ))

# function to insert horizontal line into text export
hline_fun <- function(text, table, row, new_line = FALSE) {
  if (table$metric[row] == "Total") {
    hline <- "\\hline"
    if (new_line) hline <- paste0(hline, " \\\\")
    text <- c(text, hline)
  }
  return(text)
}

# function to sink text 
# TODO: Add to functions script
sink_fun <- function(name, text) {
  sink(name, append = FALSE)
  for (i in seq_along(text)) cat(text[[i]], "\n")
  sink()
}

# function to create table rows
table_rows_fun <- function(table) {
  # create list to store rows of table
  # text <- vector(mode = "list", length = nrow(table))
  text <- c()
  # paste columns of table together, separated by " & "
  for (i in seq_len(nrow(table))) {
    # text[[i]] <- paste0(
    #   paste(apply(table[i, ], 2, paste0), collapse = " & "), 
    #   " \\\\"
    # )
    # add horizontal lines for total above and below 
    text <- hline_fun(text, table, i, FALSE)
    text <- c(
      text,
      paste0(
        "& ", 
        paste(apply(table[i, ], 2, paste0), collapse = " & "), 
        " \\\\"
      )
    )
    text <- hline_fun(text, table, i, TRUE)
  }
  return(text)
}

# split between VMMC & non-VMMC
table2a <- filter(table2, vmmc == "Non-VMMC") %>% 
  select(-vmmc)
table2b <- filter(table2, vmmc == "VMMC") %>% 
  select(-vmmc)

# create non-vmmc and vmmc text for TeX tables
sink_fun(
  name = "paper_poster_plots/paper/figures/table_1a_non_vmmc_model_spec_ppcs.txt", 
  text = table_rows_fun(table2a) 
)
sink_fun(
  name = "paper_poster_plots/paper/figures/table_1b_vmmc_model_spec_ppcs.txt", 
  text = table_rows_fun(table2b) 
)


# may also want to ignore type for VMMC countries
fit_stats_best_totals_no_type <- bind_rows(
  fit_stats_best, fit_stats_best_totals
) %>% 
  arrange(vmmc, rw_order, type, metric) %>% 
  group_by(vmmc, rw_order, metric) %>% 
  summarise(across(Neither:Both, ~ sum(.)), .groups = "drop") %>% 
  mutate(type = "Overall")

bind_rows(fit_stats_best, fit_stats_best_totals, fit_stats_best_totals_no_type) %>% 
  mutate(type = factor(type, levels = c("MC", "MMC", "TMC", "Overall"))) %>% 
  arrange(vmmc, rw_order, type, metric)


# Notes: 
# non-VMMC, rw_order 0: 
# for MC, the best model for both ELPD and RMSE was "Time TMC", while for CRPS
# "Both" and "Paed Cutoff" tied as best. 
# Ignoring the type of metric, the best model was "Time TMC", although only 
# marginally (15 vs 13 and 12, respectively) favoured over "Paed Cutoff" and 
# both
# For MMC, "Time TMC" is the best for ELPD and RMSE, while "Both" is the best 
# for ELPD. Ignoring the metric, "Time TMC" is by far the favoured model. 
# For TMC, "Time TMC" is determined to be the best model for all model fits. 
# Conclusion: "Time TMC" appears to be favoured here across all types of 
# circumcision, as the model that produces the "best" fit for the most 
# countries. 
# It is interesting that this contrasts with our analysis of the mean fit 
# statistics across all countries, perhaps suggesting that there are several 
# outlier countries that skew our mean values. 
# TODO: Investigate these outliers!
# It is also interesting that there seems to be quite some 
# disagreement between our 3 metrics for MC and MMC, while for TMC all agree. 

# non-VMMC, rw_order 1: 
# for MC, the best model for both ELPD was "Time TMC", while for CRPS and RMSE
# it was "Both". However, ignoring the type of metric, the best model overall, 
# albeit narrowly (16 vs 14 for both) was "Time TMC".
# For MMC, "Time TMC" is the best for CRPS and RMSE, while "Both" is the best 
# for ELPD. Ignoring the metric, "Time TMC" is by far the favoured model. 
# For TMC, "Time TMC" is determined to be the best model for all model fits. 
# Similar comments on agreement between metrics as for AR 1. 

# non-VMMC, rw_order 2:
# MC: Very interestingly, Time TMC is not the sole choice of model for any of 
# our three metrics, tying for the best RMSE with "Both". In total, ignoring 
# the metric, "Both" was selected as the model which produced the best fit
# in the most countries. 
# MMC: In contrast to MC, CRPS and RMSE determined "Time TMC" to be the best 
# model, while ELPD chose "Both" as the best model. Ignoring metric, "Time TMC"
# was by far chosen as the best model in the most countries. 
# TMC: Again, all metrics are in agreement that "Time TMC" is the best model. 

# Conclusion: 
# Time TMC determined to be the best model for all temporal priors and 
# 8 out of 9 temporal-prior - circumcision type combinations for non-VMMC 
# countries. 
# It is interesting that the results here (i.e. when looking at the number of 
# times a model performs "best" in each country) for AR1 is in contrast to 
# when we looked at mean fit statistics across all countries, perhaps suggesting
# there are some outliers for "Time TMC" model results which should be explored. 

# Also, when talking about models, would like to use better terms than e.g. 
# the "Time TMC" model!
# TODO: Come up with something better with Matt!

# VMMC, rw_order 0: 
# MC: CRPS and RMSE determine "Paed Cutoff" to be the best model, while 
# ELPD chooses "Neither". Ignoring metrics, "Paed Cutoff" is chosen as the best
# model. However, all four models perform reasonably well. 
# MMC: The best model by CRPS and RMSE was "Time TMC", while for ELPD it was 
# Paed Cutoff. Ignoring metrics, "Time TMC" is far superior to perform the best. 
# TMC: "Time TMC" the best by far, and for all metrics. 

# Interesting to see that there is a contrast between what model performs best
# in the most countries when estimating MC ("Paed Cutoff"), and for MMC and TMC, 
# i.e. type specific estimates ("Time TMC"). Summing across each circumcision 
# type, Time TMC is by far the most favoured model (57 vs 32 for Paed Cutoff). 
# Despite this, it is also very interesting to note that the "Both" model 
# which combines these two models does not appear to perform better than either. 
# However, it is only narrowly chosen less than "Paed Cutoff" (27 vs 29). 
# It is also very important to note that these fit statistics are based on 
# comparisons to survey estimates for 15-29 year olds (< 15 unfortunately not 
# surveyed), so we are unable to determine whether "Paed Cutoff" or "Both" 
# perform better for younger ages, where perhaps they would. 
# Also, model choice across all types is less clear than for Non-VMMC 
# countries, highlighting lesser differences in mean fit statistics in 
# model choice. 
# May also be a good idea to remove KEN and ETH, both high circumcising 
# VMMC countries, to determine if they are influential here in model choice. 
# Also for ETH look into Gambella province. 

# VMMC, rw_order 1: 
# MC: for CRPS, "Neither" and "Both" tie as the best model, for ELPD "Time TMC"
# is the best, and for RMSE "Both" is the best. Ignoring metric type, "Both" 
# narrowly best. Interesting to see a lot of disagreement between metrics here. 
# MMC: Time TMC favoured by CRPS and RMSE, ELPD favours Paed Age Cutoff
# TMC: Time TMC favoured by all. 
# Conclusion: Again, see contrast between choice for MC and MMC-TMC. Time TMC 
# still chosen when looking at best model across all metrics and types, but 
# distinction is less clear than for non-VMMC countries. 

# VMMC, rw_order 2:
# MC: Both best for CRPS & RMSE, Paed Cutoff for ELPD, totalling across metrics 
# all models perform similarly well (range from 9-11)
# MMC: Time TMC best for CRPS & RMSE, Paed Cutoff best for ELPD, Time TMC best
# overall. 
# TMC: Time TMC best across all metrics. 

# Conclusion: "Time TMC" also chosen as the best model for VMMC countries. 
# However, this choice is less "clear cut" than for non-VMMC countries, 
# particularly for MC, rather than MMC and TMC. The models with a paediadtric 
# age cutoff perform reasonably well here, even with the caveat that their 
# perform is evaluated through comparisons to survey estimates of the 15-29 age 
# group, rather than a sadly unavailable younger/wider age group of, say, 10-29, 
# where it could be argued that their performance and relative effect on models
# fit could be better ascertained. 
# Indeed, as we know that VMMC programmes do in fact refrain from performing 
# paediatric MMCs, we could easily use this background information to decide 
# to go with the "Both" model, even taking into consideration this analysis. 
# TODO: Discuss with Matt & Jeff the best model to use here!!
# Again, lots of disagreement between metrics for MC & MMC, but not TMC. 
# What do we want to prioritise here? In non-VMMC countries, MC was very high 
# already so MMC and TMC estimates were prioritised, but perhaps we'd like to 
# do something different here?
