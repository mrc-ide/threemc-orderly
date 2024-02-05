# Sendover to DMPPT2 Team (i.e. VMMC sendover (easier to search for))

#### Libraries ####

library(dplyr)
library(tidyr)
library(orderly)

#### Metadata ####

# cntry <- "BWA"
cntry <- "GHA"

vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", 
  "UGA", "MWI", "SWZ", "ZWE", "BWA",
  "ZMB", "ETH", "KEN", "ZAF"
)

rw_order = 0
inc_time_tmc = TRUE

is_paper = TRUE
if (cntry %in% c("UGA", "MWI")) {
  is_paper <- FALSE
}

paed_age_cutoff <- Inf
if (cntry %in% vmmc_iso3) {
  paed_age_cutoff = 10
}

if (!dir.exists(cntry)) {
  # threemc::create_dirs_r(dir_path = paste0(cntry, "/"))
  threemc::create_dirs_r(dir_path = paste0("vmmc_sendovers/", cntry, "/"))
}

five_year_age_groups <- c(
    "0-4", "5-9", "10-14", "15-19",
    "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "54-59"
)

age_groups <- c(
    five_year_age_groups, "0-14", "10-29", "15-29", "15-49", "30-49"
)

#### Load Data ####

# load populations
# pops_singleage <- readr::read_csv("data/population_singleage_aggr.csv.gz") %>%
#     filter(iso3 == "MOZ")
#
# age_group_df <- bind_rows(
#     lapply(age_groups, threemc:::match_age_group_to_ages)
# )
#
# pops_agegroup <- pops_singleage %>%
#     filter(age <= 50) %>%
#     left_join(age_group_df, by = "age") %>%
#     group_by(area_id, year, age_group) %>%
#     summarise(population = sum(population), .groups = "drop")

# load areas and pull through
areas_id <- orderly::orderly_list_archive() %>% 
  filter(name == "00a2_areas_join") %>% 
  slice(1) %>% 
  pull(id)
areas_orig <- areas <- sf::read_sf(file.path(
  "archive/00a2_areas_join/", areas_id, "artefacts/areas.geojson"
))
areas <- sf::st_drop_geometry(areas) %>% 
  filter(iso3 == cntry) %>% 
  distinct(area_level, area_level_label)

# Load aggregations
(id <- orderly::orderly_search(
    # "latest(parameter:cntry == cntry)",
    "latest(parameter:cntry == cntry && parameter:rw_order == rw_order &&
    parameter:inc_time_tmc == inc_time_tmc && parameter:paed_age_cutoff ==
    paed_age_cutoff && parameter:is_paper == is_paper)",
    name = "02final_aggregations",
    # name = "01final_modelling",
    parameters = list(
        cntry = cntry,
        rw_order        = rw_order,
        inc_time_tmc    = inc_time_tmc,
        paed_age_cutoff = paed_age_cutoff, 
        is_paper        = is_paper
    )
))
files <- list.files(
  file.path("archive/02final_aggregations/", id, "artefacts"), 
  full.names = TRUE
)

#### Format and Save ####

single_age_aggr <- bind_rows(lapply(files[grepl("_Age_", files)], function(x) {
    readr::read_csv(x, show_col_types = FALSE)
})) %>%
    filter(year >= 2000)
message(paste(
  "The minimum year present in this data is", 
  min(single_age_aggr$year)
))

age_group_aggr <- bind_rows(lapply(files[grepl("_AgeGroup_", files)], function(x) {
    readr::read_csv(x, show_col_types = FALSE)
})) %>%
    filter(year >= 2000)


# Pull through circumcision coverage, circumcision rates, and unmet need
single_age_aggr_final <- single_age_aggr %>%
    select(-model) %>%
    filter(
        type %in% c(
            unlist(lapply(c("MC", "MMC", "TMC"), paste, c("coverage", "probability"))),
            "Unmet need"
        )
    ) %>%
    mutate(
        type = ifelse(type == "Unmet need", "uncircumcised", type),
        type = ifelse(
            grepl("probability", type),
            paste0(stringr::str_remove(string = type, pattern = "probability"), "rate"),
            type
        )
    ) %>%
    relocate(population, .after = age) %>%
    arrange(type, area_id, year, age) %>%
    # mutate(
    #     area_level_label = case_when(
    #       area_level == 0 ~ "Country",
    #       area_level == 1 ~ "Province",
    #       TRUE            ~ "District"
    #     )
    # ) %>%
    left_join(areas, by = "area_level") %>% 
    relocate(area_level_label, .after = area_level)

age_group_aggr_final <- age_group_aggr %>%
    select(-model) %>%
    filter(
        type %in% c(
            unlist(lapply(c("MC", "MMC", "TMC"), paste, c("coverage", "probability"))),
            "Unmet need"
        ),
        age_group %in% age_groups
    ) %>%
    mutate(
        type = ifelse(type == "Unmet need", "uncircumcised", type),
        type = ifelse(
            grepl("probability", type),
            paste0(stringr::str_remove(string = type, pattern = "probability"), "rate"),
            type
        )
    ) %>%
    # left_join(pops_agegroup) %>%
    relocate(population, .after = "age_group") %>%
    # arrange(type, area_id, year)
    identity()

# arrange age groups correctly
age_group_aggr_final$age_group <- factor(
    age_group_aggr_final$age_group, levels = age_groups
)

age_group_aggr_final <- age_group_aggr_final %>%
    arrange(type, area_id, year, age_group) %>%
    # mutate(
    #     area_level_label = case_when(
    #         area_level == 0 ~ "Country",
    #         area_level == 1 ~ "Province",
    #         TRUE            ~ "District"
    #     )
    # ) %>%
    identity()

# join in area_level_label
areas_lev_lab <- sf::st_drop_geometry(areas_orig) %>% 
  select(area_id, area_level_label) %>% 
  filter(area_id %in% age_group_aggr_final$area_id) %>% 
  distinct()

single_age_aggr_final <- left_join(
  select(single_age_aggr_final, -matches("area_level_label")), 
  areas_lev_lab
) %>% 
  relocate(area_level_label, .after = area_level)
age_group_aggr_final <- left_join(
  select(age_group_aggr_final, -matches("area_level_label")), 
  areas_lev_lab
) %>% 
  relocate(area_level_label, .after = area_level)


# save
readr::write_csv(
  # filter(single_age_aggr_final, area_level != 1), # remove area level 1, same as area level 2
  single_age_aggr_final,
  # "plots_extract/MOZ_age_sendover.csv"
  paste0("vmmc_sendovers/", cntry, "/", cntry, "_age_sendover.csv")
  # "UGA/UGA_age_sendover.csv"
)
readr::write_csv(
  # filter(age_group_aggr_final, area_level != 1), 
  age_group_aggr_final, 
  # "plots_extract/MOZ_agegroup_sendover.csv"
  paste0("vmmc_sendovers/", cntry, "/", cntry, "_agegroup_sendover.csv")
  # "UGA/UGA_agegroup_sendover.csv"
)
