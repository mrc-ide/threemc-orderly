#### Recreate Matt's table 4-6 from supp. figs, with mean vals ####

# Much of code from Adam Howe's AGYW analysis

# First, run "ppc_scripts/01_model_spec_comparisons.R", to get "fit_stats_join"

#### Libs ####

library(dplyr)
library(ggplot2)
library(tidyr)

#### Metadata ####

# vmmc countries
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" 
)

#### Initial ####

fit_stats_join <- readr::read_csv("01e_new_final_ppc_models_fit_stats.csv")

# Label VMMC status
fit_stats_join <- fit_stats_join %>% 
  mutate(vmmc = ifelse(cntry %in% vmmc_iso3, "VMMC", "Non-VMMC")) %>% 
  relocate(vmmc, .after = type)

# initial preprocessing
fit_stats_join1 <- fit_stats_join %>% 
  # select(-c(mae, contains("ppd"))) %>% 
  # select(-c(mae, elpd)) %>% 
  select(-elpd) %>% # keep mae
  mutate(
    mod = case_when(
      paed_age_cutoff == Inf & inc_time_tmc == TRUE ~
        "Time TMC",
      paed_age_cutoff == Inf & inc_time_tmc == FALSE ~
        "Neither",
      paed_age_cutoff == 10 & inc_time_tmc == TRUE ~
        "Both",
      paed_age_cutoff == 10 & inc_time_tmc == FALSE ~
        "Paed Cutoff", 
      TRUE ~ "Unknown"
    ), 
    mod = factor(
      mod, levels = c("Neither", "Time TMC", "Paed Cutoff", "Both")
    )
  )


#### Calculate Means ####

# calculate mean (or median?) across all countries
fit_stats_mean <- fit_stats_join1 %>%
  # summarise, removing country
  group_by(type, vmmc, rw_order, mod) %>% 
  summarise(
    # across(matches(c("crps", "elpd", "rmse")), ~ mean(., na.rm = TRUE)),
    # across(matches(c("crps", "rmse")) | contains("ppd"), ~ mean(., na.rm = TRUE)),
    across(matches(c("crps", "mae", "rmse")) | contains("ppd"), ~ mean(
      ., na.rm = TRUE
    )),
    .groups = "drop"
  ) %>% 
  rename(
    # "elpd_mean" = "elpd", 
    "crps_mean" = "crps", 
    "mae_mean"  = "mae",
    "rmse_mean" = "rmse",
    "ppd0.500_mean" = "ppd_0.500",
    "ppd0.800_mean" = "ppd_0.800",
    "ppd0.950_mean" = "ppd_0.950"
  ) %>% 
  # pivot_longer(cols = starts_with(c("crps", "rmse", "ppd"))) %>% 
  pivot_longer(cols = starts_with(c("crps", "mae", "rmse", "ppd"))) %>% 
  separate(
    name, 
    into = c("metric", "stat"), 
    extra = "merge", 
    fill = "left", 
    sep = "_"
  ) %>%
  # add back in underscore to coverage variable names
  mutate(
    metric = case_when(
      metric == "ppd0.500" ~ "ppd_0.500",
      metric == "ppd0.800" ~ "ppd_0.800",
      metric == "ppd0.950" ~ "ppd_0.950", 
      TRUE               ~ metric
    )
  ) %>% 
  pivot_wider(
    names_from  = "stat",
    values_from = "value"
  )

#### Tabulate Best Models and Fit Stats ####

# Find best model for each metric & rw_order/type/vmmc combination
fit_stats_best <- fit_stats_mean %>% 
  # for PPDs, calculate difference from ideal value
  mutate(
    diff_0.500 = abs(mean - 0.5),
    diff_0.800 = abs(mean - 0.8),
    diff_0.950 = abs(mean - 0.95)
  ) %>% 
  # split by each metric & rw_order/type/vmmc combination
  group_split(metric, rw_order, type, vmmc) %>%
  lapply(function(x)
    x %>%
      mutate(
        # find where minimum and maximum metric values are
        min_idx = (mean == min(mean, na.rm = TRUE)),
        max_idx = (mean == max(mean, na.rm = TRUE)),
        # for CIs, find where metric value is closest to e.g. 50% percentile
        closest_idx = case_when(
          metric == "ppd_0.500" ~ (diff_0.500 == min(diff_0.500, na.rm = TRUE)),
          metric == "ppd_0.800" ~ (diff_0.800 == min(diff_0.800, na.rm = TRUE)),
          metric == "ppd_0.950" ~ (diff_0.950 == min(diff_0.950, na.rm = TRUE)),
          TRUE                  ~ NA
        ),
        # for mean fit best mod has min vals, for CIs is closest to target %ile
        best_idx = case_when(
          # metric %in% c("rmse", "crps") ~ min_idx, 
          metric %in% c("crps", "mae", "rmse") ~ min_idx, 
          TRUE                                 ~ closest_idx
        )
      )
  ) %>%
  bind_rows() %>% 
  # Change label text
  mutate(
    # metric = toupper(metric), 
    # type = stringr::str_remove(type, " coverage"), 
    best_mod = ifelse(best_idx, as.character(mod), NA)
  ) %>% 
  # tabulate nicely
  select(vmmc, rw_order, type, metric, mod, mean, best_mod) %>% 
  group_by(vmmc, rw_order, type, metric) %>% 
  tidyr::fill(best_mod, .direction = "updown") %>% 
  ungroup() %>% 
  pivot_wider(names_from = mod, values_from = mean) %>% 
  relocate(best_mod, .after = everything()) %>% 
  arrange(vmmc, rw_order, type, metric)


#### Old: Match Matt's layout ####

# change df to match Matt's (and original lol) structure
fit_stats_tbl <- fit_stats_best %>% 
  # select(-best_mod) %>% 
  tidyr::pivot_longer(Neither:Both, names_to = "mod", values_to = "mean") %>% 
  mutate(
    paed_age_cutoff = case_when(
     mod %in% c("Neither", "Time TMC")  ~ Inf,
     TRUE                               ~ 10
    ),
    inc_time_tmc = case_when(
     mod %in% c("Time TMC", "Both") ~ TRUE,
     TRUE                           ~ FALSE
    )
  )

# extract best mods
best_mods <- fit_stats_tbl %>% 
  distinct(
    vmmc, rw_order, paed_age_cutoff, inc_time_tmc, type, metric, best_mod
  ) %>% 
  # label best mod with 1 or 0, rather than name 
  mutate(
    # TODO: functionalise this! Used many times (also converse)
    mod = case_when(
      paed_age_cutoff == Inf & inc_time_tmc == TRUE ~
        "Time TMC",
      paed_age_cutoff == Inf & inc_time_tmc == FALSE ~
        "Neither",
      paed_age_cutoff == 10 & inc_time_tmc == TRUE ~
        "Both",
      paed_age_cutoff == 10 & inc_time_tmc == FALSE ~
        "Paed Cutoff", 
      TRUE ~ "Unknown"
    ), 
    best_mod = ifelse(best_mod == mod, 1, 0)
  ) %>% 
  select(-mod)

# widen fit statistics for each model
fit_stats_tbl <- fit_stats_tbl %>% 
  select(-best_mod) %>% 
  mutate(metric = tolower(metric)) %>% 
  tidyr::pivot_wider(names_from = metric, values_from = mean) %>% 
  # select(rw_order, paed_age_cutoff, inc_time_tmc, type, best_mod, CRPS:RMSE)
  select(vmmc, rw_order, paed_age_cutoff, inc_time_tmc, type, crps:rmse) %>% 
  as.data.frame()

# function to change model specification labels
change_labels <- function(df) {
  ret_df <- df %>% 
    mutate(
    rw = case_when(
      rw_order == 0 ~ "AR1",
      rw_order == 1 ~ "RW1",
      rw_order == 2 ~ "RW2"
    ),
    paed = case_when(
      paed_age_cutoff == "10"  ~ "Paediatric cut-off",
      paed_age_cutoff == "Inf" ~ "No cut-off"
    ),
    type = case_when(
      type == "MC coverage"  ~ "MC",
      type == "TMC coverage" ~ "MMC",
      type == "MMC coverage" ~ "TMC",
      TRUE                   ~ type
    ),
    tmc = case_when(
      inc_time_tmc == TRUE  ~ "Time variant",
      inc_time_tmc == FALSE ~ "Time invariant"
    )
  ) %>% 
    select(-c(rw_order, paed_age_cutoff, inc_time_tmc))
  
  return(ret_df)
}


# Matt's code from here

# Function to format table as Matt has it to export to TeX
tex_table_layout <- function(fit_stats_tbl, vmmc_status) {
  # filter for Non-VMMC or VMMC only countries
  fit_stats_tbl <- fit_stats_tbl %>% 
    filter(vmmc == vmmc_status)
  
  # Change coverage to percentage
  fit_stats_tbl[, c("ppd_0.500", "ppd_0.800", "ppd_0.950")] <- 100 * 
    fit_stats_tbl[, c("ppd_0.500", "ppd_0.800", "ppd_0.950")]
  
  # Format dataset for output
  fit_stats_tbl <- fit_stats_tbl %>%
    # Alter labels 
    change_labels() %>% 
    # select relevant columns
    dplyr::select(
      # rw, paed, type, tmc, crps, rmse, ppd_0.500, ppd_0.800, ppd_0.950
      rw, paed, type, tmc, crps, mae, rmse, ppd_0.500, ppd_0.800, ppd_0.950
    ) %>%
    # wide to long dataset
    reshape2::melt(id.vars = c("rw", "paed", "type", "tmc"))
  
  # add best models
  fit_stats_tbl <- left_join(
    fit_stats_tbl, 
    change_labels(best_mods) %>% 
      # filter(vmmc == "Non-VMMC") %>% 
      filter(vmmc == vmmc_status) %>% 
      mutate(variable = tolower(metric)) %>% 
      select(-c(vmmc, metric))
  ) %>% 
    # Format for table output
    dplyr::mutate(value = format(round(value, 2), nsmall = 2)) %>%
    dplyr::mutate(value = if_else(
      best_mod == 1, paste0("\\bf", value), value)
    ) %>%
    # remove best model variable
    dplyr::select(-c(best_mod)) %>%
    # Relabel datset 
    mutate(variable = paste(tmc, variable, sep = " "))%>%
    # select relevant columns
    dplyr::select(-c(tmc)) %>%
    # Long to wide dataset
    pivot_wider(names_from = variable, values_from = value) %>% 
    # Dummy data to add lines into tables
    # plyr::rbind.fill(expand.grid(rw = c("AR1", "RW1", "RW2"),
    #                        paed = c("Paediatric cut-off"), #, "No cut-off"),
    #                        type = "XXX")) %>%
    # Sort dataset
    arrange(rw, paed, type) 
  
  # Removing dummy data
  fit_stats_tbl$paed[duplicated(fit_stats_tbl[, c("rw", "paed")])] <- ""
  fit_stats_tbl$rw[duplicated(fit_stats_tbl$rw)] <- ""
  fit_stats_tbl[which(fit_stats_tbl$type == "XXX"), ] <- ""
  
  # Removing unnecessary columns (???)
  # fit_stats_tbl$`Time invariant rmse` <- NULL
  # fit_stats_tbl$`Time variant rmse` <- NULL
  
  # reorder so time invariant columns come before time variant, as in table
  fit_stats_tbl <- fit_stats_tbl %>% 
    relocate(contains("Time invariant"), .after = "type")
  
  return(fit_stats_tbl)
}

# calculate table for both VMMC and non-VMMC countries
fit_stats_tbl_vmmc <- tex_table_layout(fit_stats_tbl, "VMMC") 
fit_stats_tbl_non_vmmc <- tex_table_layout(fit_stats_tbl, "Non-VMMC") 

#### Old: Print Latex Table to .txt file ####

# function to print latex table to .txt file
latex_export <- function(fit_stats_tbl, vmmc_status, name) {
  
  # open text file
  sink(name, append = FALSE)
  
  cat("\n")
  cat("{\\linespread{1}", "\n")
  cat("  \\begin{table}[H]", "\n")
  cat("  \\centering", "\n")
  cat("  \\footnotesize", "\n")
  # cat("  \\begin{tabular}{>{\\bfseries}p{0.75cm} 
  #     >{\\bfseries}p{3cm} p{1cm} C{1.25cm} 
  #     C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} 
  #     C{1.25cm} C{1.25cm}}", "\n")
  cat("  \\begin{tabular}{>{\\bfseries}p{0.75cm} 
     >{\\bfseries}p{3cm} p{1cm} C{1.25cm} 
     C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm} 
     C{1.25cm} C{1.25cm} C{1.25cm} C{1.25cm}}", "\n")
  cat("  \\hline ", "\n")
  # cat("  & & & \\multicolumn{10}{c}{\\bf TMC Model} \\\\", "\n")
  cat("  & & & \\multicolumn{12}{c}{\\bf TMC Model} \\\\", "\n")
  # cat("  \\cmidrule(lr){4-13}", "\n")
  cat("  \\cmidrule(lr){4-15}", "\n")
  # cat("  & & & \\multicolumn{5}{c}{\\bf Time invariant} & 
  #     \\multicolumn{5}{c}{\\bf Time variant} \\\\", "\n")
  cat("  & & & \\multicolumn{6}{c}{\\bf Time invariant} & 
      \\multicolumn{6}{c}{\\bf Time variant} \\\\", "\n")
  # cat("  \\cmidrule(lr){4-8}", "\n")
  cat("  \\cmidrule(lr){4-9}", "\n")
  # cat("  \\cmidrule(lr){9-13}", "\n")
  cat("  \\cmidrule(lr){10-15}", "\n")
  # cat("  & {\\bf MMC Model} & {\\bf Type} & {\\bf CRPS} & {\\bf RMSE} & 
  #     {\\bf 50\\% CI} & {\\bf 80\\% CI} & {\\bf 95\\% CI} & {\\bf CRPS} & 
  #     {\\bf RMSE} & {\\bf 50\\% CI} & {\\bf 80\\% CI} & {\\bf 95\\% CI} \\\\", 
  #     "\n")
  cat("  & {\\bf MMC Model} & {\\bf Type} & 
      {\\bf CRPS} & {\\bf MAE} & {\\bf RMSE} & 
      {\\bf 50\\% CI} & {\\bf 80\\% CI} & {\\bf 95\\% CI} & 
      {\\bf CRPS} & {\\bf MAE} & {\\bf RMSE} & 
      {\\bf 50\\% CI} & {\\bf 80\\% CI} & {\\bf 95\\% CI} \\\\", 
      "\n")
  cat("  \\hline", "\n")
  # table contents
  cat(paste(paste(
    fit_stats_tbl_vmmc[, 1, drop = TRUE], 
    fit_stats_tbl_vmmc[, 2, drop = TRUE], 
    fit_stats_tbl_vmmc[, 3, drop = TRUE], 
    fit_stats_tbl_vmmc[, 4, drop = TRUE], 
    fit_stats_tbl_vmmc[, 5, drop = TRUE], 
    fit_stats_tbl_vmmc[, 6, drop = TRUE], 
    fit_stats_tbl_vmmc[, 7, drop = TRUE], 
    fit_stats_tbl_vmmc[, 8, drop = TRUE], 
    fit_stats_tbl_vmmc[, 9, drop = TRUE], 
    fit_stats_tbl_vmmc[, 10, drop = TRUE], 
    fit_stats_tbl_vmmc[, 11, drop = TRUE], 
    fit_stats_tbl_vmmc[, 12, drop = TRUE], 
    fit_stats_tbl_vmmc[, 13, drop = TRUE], 
    fit_stats_tbl_vmmc[, 14, drop = TRUE], 
    fit_stats_tbl_vmmc[, 15, drop = TRUE], 
    sep = " & "
  ), "\\\\ \n")) 
  cat("  \\hline", "\n")
  cat("  \\end{tabular}", "\n")
  # cat(paste0("  \\caption{Results of the posterior predictive checking in total 
  #            male circumcision (MC), medical male circumcision (MMC) and 
  #            traditional male circumcision (TMC) from fitting the 12 candidate 
  #            models and averaging across all ",
  cat(paste0("  \\caption{Results of the posterior predictive checking in total 
             male circumcision (MC), medical male circumcision (MMC) and 
             traditional male circumcision (TMC) from fitting the 12 candidate 
             models and taking the median value across all ",
             #"Non-VMMC countries",
             paste0(vmmc_status, " countries"),
             ". Combinations include 
             (i) Time invariant or Time variant TMC, 
             (ii) No cut off vs. Paediatric cut-off in MMC, and 
             (iii) Autoregressive order 1 (AR1), Random Walk 1 (RW1) or 
             Random Walk 2 (RW2) temporal prior. 
             For all combinations, the within-sample continuous ranked 
             probability scores (CRPS), mean absolute error (MAE) , 
             root mean squared error (RMSE), and the proportion of empirical 
             observations that fell within the 50\\%, 80\\%, and 95\\% 
             quantiles are shown.}"), "\n")
  # cat(paste0("  \\label{tab::PPC1", i, "}"), "\n")
  # cat(paste0("  \\label{tab::PPC1", "Non-VMMC", "}"), "\n")
  x <- 1
  if (vmmc_status == "Non-VMMC") x <- 2
  # cat(paste0("  \\label{tab::PPC1", vmmc_status, "}"), "\n")
  cat(paste0("  \\label{tab::PPC", x, vmmc_status, "}"), "\n")
  cat("\\end{table}}", "\n")
  cat("\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n")
  
  # Close text file
  sink()
}

# export for both VMMC and non-VMMC
latex_export(
  fit_stats_tbl_vmmc, 
  "VMMC", 
  "01_model_spec_table_vmmc.txt"
)
latex_export(
  fit_stats_tbl_non_vmmc, 
  "Non-VMMC", 
  "02_model_spec_table_non_vmmc.txt"
)
