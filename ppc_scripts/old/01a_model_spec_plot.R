#### Dotplot average fit statistics for each model spec ####

# stole from Adam https://github.com/athowes/multi-agyw/blob/0089243647f7a03d6aad0d023ab47272031f7428/src/process_information-criteria/script.R

# preprocess information
plt_dat <- fit_stats_join %>%
  select(-c(mae, contains("ppd"))) %>% 
  # summarise, removing country
  group_by(type, vmmc, rw_order, paed_age_cutoff, inc_time_tmc) %>% 
  summarise(
    across(matches(c("crps", "elpd", "rmse")), ~ mean(., na.rm = TRUE)),
    .groups = "drop"
  ) %>% 
  # rename("dic_mean" = "dic", "waic_mean" = "waic", "cpo_mean" = "cpo") %>%
  rename("elpd_mean" = "elpd", "crps_mean" = "crps", "rmse_mean" = "rmse") %>% 
  # pivot_longer(
  #   cols = starts_with(c("dic", "waic", "cpo")),
  #   names_to = "name",
  #   values_to = "value"
  # ) %>%
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
  ) %>%
  group_split(metric, rw_order, type, vmmc) %>%
  lapply(function(x)
    x %>%
      mutate(
        min_idx = (mean == min(mean, na.rm = TRUE)),
        max_idx = (mean == max(mean, na.rm = TRUE)),
        # best_idx = ifelse(metric %in% c("WAIC", "DIC"), min_idx, max_idx)
        best_idx = ifelse(metric %in% c("rmse", "crps"), min_idx, max_idx)
      )
  ) %>%
  bind_rows() %>% 
  # Change label text
  mutate(
    metric = toupper(metric), 
    type = stringr::str_remove(type, " coverage")
  ) %>% 
  # group_split(rw_order, type, vmmc) %>% 
  group_split(vmmc, rw_order) %>% 
  identity()

labels <- rep(c("AR1", "RW1", "RW2"), 2)

plots <- lapply(seq_along(plt_dat), function(i) {
  
  p <- plt_dat[[i]] %>% 
    ggplot(aes(x = mod, y = mean, col = mod, shape = best_idx)) +
    geom_point(size = 3) +
    # geom_errorbar(
    #   aes(ymin = mean - se, ymax = mean + se),
    #   stat = "identity", position = "dodge", alpha = 0.4, col = "black", width = 0
    # ) +
    facet_grid(metric ~ type, scales = "free") + 
    # facet_grid(type ~ metric, scales = "free_y") +
   # scale_color_manual(values = multi.utils::cbpalette()) +
    scale_y_continuous(labels = scales::label_number(accuracy = 2)) + 
    ggsci::scale_color_nejm() + 
    scale_shape_manual(values = c(16, 15)) +
    guides(shape = "none") +
    labs(y = "Value", x = "", col = "Model Specification:", tag = labels[i]) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(), 
      axis.text.y = element_text(size = rel(1.5)),
      axis.title.y = element_text(size = rel(1.5)),
      plot.tag = element_text(size = rel(1.5), face = "bold"),
      strip.text = element_text(size = rel(1.5)),
      # plot.grid.x = element_blank(),
      legend.margin = margin(0, 0, 0, 0)
    )
  
  # for outer plots, remove legend and have no title
  if (!i %in% c(2, 5)) {
    p <- p + 
      guides(colour = "none")
  } else {
    p <- p + 
      ggtitle(unique(plt_dat[[i]]$vmmc)) + 
      theme(plot.title = element_text(size = rel(1.5), hjust = 0.5))
  }
  
  return(p)
})

# 

lapply(plt_dat, function(x) {x %>% janitor::tabyl(mod, best_idx, type)})

# Non-VMMC 
gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow = 1)

# Conclusions: 
# For AR1 temporal prior: 


# VMMC
gridExtra::grid.arrange(plots[[4]], plots[[5]], plots[[6]], nrow = 1)
