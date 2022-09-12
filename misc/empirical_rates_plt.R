#### test plot for empirical rates vs modelled rates ####

#### Initial ####

library(dplyr)
library(tidyr)
library(ggplot2)

source("Shiny/src/functions.R")

cntry <- "MWI"
# cntry <- "MWI"

age_groups <- c(
  "0-4",   "5-9",   "10-14", "15-19",
  "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "54-59"
)

latest_id <- orderly::orderly_list_archive() %>% 
  filter(name == "03_shiny_consolidation") %>% 
  slice(n()) %>% 
  pull(id)

results <- readr::read_csv(file.path(
  "archive/03_shiny_consolidation",
  latest_id,
  "artefacts/results_agegroup.csv.gz"
)) %>% 
  filter(iso3 == cntry, age_group %in% age_groups)

if (nrow(results) == 0) results <- NULL

# empirical_rates <- readr::read_csv(file.path(
#   "archive/03_shiny_consolidation",
#   latest_id,
#   "artefacts/empirical_rates.csv.gz"
# )) %>% 
#   filter(iso3 == cntry, age_group %in% age_groups)
empirical_rates_task <-orderly::orderly_list_archive() %>% 
  filter(name == "01b_empirical_rates") %>% 
  slice(n()) %>% 
  pull(id)
empirical_rates <- readr::read_csv(file.path(
  "archive/01b_empirical_rates",
  empirical_rates_task,
  "artefacts/empirical_rates.csv.gz"
)) %>% 
  filter(iso3 == cntry, age_group %in% age_groups)


surveys <- readr::read_csv(file.path(
  "archive/03_shiny_consolidation",
  latest_id,
  "artefacts/survey-circumcision-coverage_shiny.csv.gz"
)) %>% 
  filter(iso3 == cntry)

areas <- sf::read_sf(file.path(
  "archive/03_shiny_consolidation",
  latest_id,
  "depends/areas.geojson"
)) %>% 
  sf::st_drop_geometry() %>% 
  filter(iso3 == cntry)


#### Test Plot ####

survey_years <- surveys %>%
  mutate(year = as.numeric(substr(survey_id, 4, 7))) %>%
  distinct(year) %>%
  pull()

if (length(survey_years) == 0) {
  survey_years <- empirical_rates %>% 
    filter(mean != 0) %>% 
    distinct(year) %>% 
    pull()
}

# # take age above age_groups as last label for plot
# final_label <- last(age_groups)
# final_label <- as.numeric(stringr::str_split(final_label, "-")[[1]][[2]])
# final_label <- paste0(final_label + 1, "+")
# 
# results <- results %>%
#   filter(
#     area_id == cntry,
#     year == last(survey_years) - 1,
#     type == "MMC probability"
#   ) %>%
#   mutate(indicator = "Model Rates")
# 
# empirical_rates <- empirical_rates %>%
#   filter(
#     area_id == cntry, 
#     year == last(survey_years) - 1, 
#     type == "MMC probability"
#   ) %>%
#   mutate(indicator = "Empirical Rates")

p1 <- plt_empirical_model_rates(
  empirical_rates,
  # df_results = results, 
  spec_area_levels = 0,
  spec_type = "MMC probability",
  spec_age_groups = age_groups, 
  spec_years = survey_years
)
p1[[1]]


plt_empirical_model_rates(
  empirical_rates %>% 
    filter(!age_group %in% c("0-4", "5-9", "50-54", "54-59")),
  # df_results = results, 
  spec_area_levels = 0,
  spec_type = "MMC probability",
  spec_age_groups = age_groups, 
  spec_years = survey_years[1]:2016,
  xlab = "",
  ylab = "% Circumcised Per Year"
)[[1]] + 
  ggtitle("Malawi Empirical Rates vs Year, by Age Group") + 
  facet_wrap(~ age_group, nrow = 2) + 
  scale_x_discrete(
    breaks = seq(survey_years[1], 2016, by = 2)
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 13, face = "bold"),
    axis.text.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 20)
  ) 