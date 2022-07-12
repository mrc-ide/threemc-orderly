#### Coalating Population data for each country

#### Initial ####

save_dir <- "artefacts/"
depends_path <- "depends/"
threemc::create_dirs_r(save_dir)

# age groups to calculate populations for 
age_groups <- c(
  "0-4", "5-9", "10-14", "15-19",
  "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "54-59",
  "0+", "10+", "15+", "15-24", "10-24",
  "15-29", "10-29", "15-39",
  "10-39", "15-49", "10-49"
)

# load survey clusters, to identify missing area levels to use
survey_circumcision <- read_circ_data(
    paste0(depends_path, "survey_circumcision.csv.gz"),
    filters = c(sex = "male")
)

# load shapefile
areas <- read_circ_data(path = paste0(depends_path, "areas.geojson")) %>%
    # Add a unique identifier within Admin code and merging to boundaries
    sf::st_drop_geometry() %>%
    group_by(iso3, area_level) %>%
    mutate(space = row_number()) %>%
    ungroup()

# wide formatted areas, for changing area levels later
areas_wide <- areas %>%
    select(area_id, area_name, parent_area_id, area_level) %>%
    naomi::spread_areas()

areas_wide <- areas_wide[
  , !colSums(apply(areas_wide, 2, is.na)) == nrow(areas_wide)
]

# Load populations (for male pop for country in question only)
populations <- read_circ_data(
    # here::here("data/population_singleage.csv.gz"),
    paste0(depends_path, "population_singleage.csv.gz"),
    filters = c("sex" = "male")
)

# remove NAs in population
populations <- populations %>%
    filter(!is.na(population))

# pull recommended area hierarchy for each country
area_lev_df <- threemc::datapack_psnu_area_level %>%
    filter(psnu_area_level > 0) # don't model at the country level

# if area_level is missing, assume most common area lev in survey clusters
missing_countries <- unique(populations$iso3)
missing_countries <- missing_countries[!missing_countries %in% area_lev_df$iso3]

# if area_level is missing, assume most common area lev in surveys
if (length(missing_countries) > 0) {
    missing_area_levs <- survey_circumcision %>%
        filter(iso3 %in% missing_countries) %>%
        mutate(area_level = as.numeric(substr(area_id, 5, 5))) %>%
        group_by(iso3) %>%
        count(area_level) %>%
        filter(n == max(n, na.rm = TRUE)) %>%
        ungroup() %>%
        select(iso3, psnu_area_level = area_level)
    area_lev_df <- rbind(area_lev_df, missing_area_levs)
}

# remove unneeded cols from areas_wide
# keep_cols <- seq_len(last(which(grepl(
#     max(missing_area_levs$psnu_area_level), names(areas_wide)
# ))))
# areas_wide <- areas_wide[, keep_cols]

# find countries which don't have populations for all area hierarchies
missing_pop_countries <- populations %>%
    # take distinct area levels in pops for each country
    group_by(iso3) %>%
    distinct(area_level) %>%
    ungroup() %>%
    # number of rows for each country should be >= psnu_area_level, else
    # there are areas for the countries in question with no populations!
    count(iso3) %>%
    left_join(area_lev_df) %>%
    filter(
        !is.na(psnu_area_level),
        n < (psnu_area_level + 1) # need n to be 0 indexed, like area_lev
    )

if (nrow(missing_pop_countries) > 0) {
    # pull rows in populations for countries with missing populations
    missing_pops_list <- populations %>%
        # take highest area level for each country
        group_by(iso3) %>%
        filter(area_level == max(area_level, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(iso3 %in% missing_pop_countries$iso3) %>%
        # spliy by iso3 code
        split(.$iso3)
    cntries <- names(missing_pops_list)

    # collect populations for lower area hierarchies for each country
    missing_pops_list <- lapply(seq_along(missing_pops_list), function(i) {
        cntry <- names(missing_pops_list)[i]
        print(cntry)

        # check that the max area level in populations matches the PSNU level
        spec_area_lev <- area_lev_df %>%
            filter(iso3 == cntry) %>%
            pull(psnu_area_level)
        max_area_lev <-  max(missing_pops_list[[i]]$area_level)
        
        if (spec_area_lev < max_area_lev) {
            message(paste0(
                "PSNU area level < max area_level in populations for ",
                cntry,
                ", changing accordingly from ",
                spec_area_lev,
                " to ",
                max_area_lev
            ))
            spec_area_lev <- max_area_lev
        }

        threemc:::combine_areas(
            missing_pops_list[[i]],
            areas_wide = areas_wide %>% filter(area_id0 == cntry),
            area_lev = spec_area_lev,
            join = TRUE,
            add_keep_cols = names(populations),
            fill = TRUE
        )
    })
    names(missing_pops_list) <- cntries

    # rejoin and reorder to be the same as in populations
    missing_pops_df <- as.data.frame(
        data.table::rbindlist(missing_pops_list, use.names = TRUE)
    ) %>%
        # remove duplicate rows
        distinct() %>%
        select(all_of(names(populations))) %>%
        # aggregate populations in lower area hierarchies by summing populations
        group_by(across(iso3:age)) %>%
        summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

    # add missing populations to original populations
    populations <- populations %>%
        filter(!iso3 %in% missing_pops_df$iso3) %>%
        bind_rows(missing_pops_df) %>%
        arrange(iso3, area_id, year, age)
}

# remove any duplicate rows
pops_final <- populations %>%
    distinct(area_id, year, sex, age, .keep_all = TRUE) %>%
    # remove any areas where area_level does not match area_id
    filter(
        (area_level == 0) | (as.numeric(substr(area_id, 5, 5)) == area_level)
    )

# remove columns with all NAs
# pops_final <- pops_final[, colSums(is.na(pops_final)) < nrow(pops_final)]
pops_final <- Filter(function(x) !all(is.na(x)), pops_final)

# pop_test <- readr::read_csv("data/population_singleage_aggr_orig.csv.gz")
#
# pop_test <- populations %>%
#     left_join(
#         pop_test %>%
#             rename(population_test = population),
#         by = c("iso3", "area_id", "area_level", "area_name", "year", "sex", "age")
#     ) %>%
#     filter(round(population) != round(population_test))

# readr::write_csv(pops_final, "data/population_singleage_aggr.csv.gz")
readr::write_csv(
  pops_final, paste0(save_dir, "population_singleage_aggr.csv.gz")
)

#### Single Ages to Age Groups for populations ####

# TODO: Change below so there's no need for a list here
pops_final_agegroup_list <- list(pops_final)

# aggregate sample for each age group
pops_final_agegroup <- lapply(seq_along(age_groups), function(i) {
  # If upper limit use this split
  if (grepl("-", age_groups[i])) {
    age1 <- as.numeric(strsplit(age_groups[i], "-")[[1]][1])
    age2 <- as.numeric(strsplit(age_groups[i], "-")[[1]][2])
  }
  # If no upper limit use this split
  if (grepl("\\+", age_groups[i])) {
    age1 <- as.numeric(strsplit(age_groups[i], "\\+")[[1]][1])
    age2 <- Inf
  }
  results_list_loop <- lapply(pops_final_agegroup_list, function(x) {
    x <- x %>%
      # take results for age group i
      dplyr::filter(age >= age1, age <= age2) %>%
      dplyr::select(-matches("age"))
    # Getting summarising samples
    x <- data.table::setDT(x)[,
                              lapply(.SD, sum, na.rm = T),
                              by = c("iso3",      "area_id", "area_level",
                                     "area_name", "year",    "sex"),
                              .SDcols = "population"
    ]
    x <- x %>%
      # Adding age group
      dplyr::mutate(age_group = age_groups[i])
  })
  # Printing index
  print(age_groups[i])
  # return ages
  return(results_list_loop)
})
# join together
pops_final_agegroup <- bind_rows(rlang::squash(pops_final_agegroup))

# save
readr::write_csv(
  pops_final_agegroup, file.path(save_dir, "population_agegroup_aggr.csv.gz")
)
