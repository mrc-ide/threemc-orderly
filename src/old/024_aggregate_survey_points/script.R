#### Survey data plotting preparation

# source aggregation functions
source("source.R")

# Revert to using planar rather than spherical geometry in `sf`
sf_use_s2(FALSE)

# choose country
# cntry <- "ETH"

# location to save results in
save_loc <- "circumcision_surveypoints.csv.gz"

################################################
### Load Data ###
################################################

### Metadata to run the models

k_dt <- 5 # Age knot spacing
start_year <-  2006
cens_age = 59

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# read in data, filter for specific country and male surveys only
filters <- c("iso3" = cntry, sex = "male")
areas <- read_circ_data("areas.geojson", filters)
survey_clusters <- read_circ_data("survey_clusters.csv.gz", filters)
survey_individuals <- read_circ_data("survey_individuals.csv.gz", filters)
survey_circumcision <- read_circ_data("survey_circumcision.csv.gz", filters)


areas_wide <- areas %>%
  st_drop_geometry() %>%
  naomi::spread_areas()

# pull recommended area hierarchy for target country
area_lev <- threemc::datapack_psnu_area_level %>%
    filter(iso3 == cntry) %>%
    pull(psnu_area_level)
if (area_lev == 0) area_lev <- NULL # don't model at the country level

# if area_level is missing, assume most common area lev in surveys
if (length(area_lev) == 0) {
    area_lev <- table(as.numeric(substr(survey_clusters$geoloc_area_id, 5, 5)))
    area_lev <- as.numeric(names(area_lev)[area_lev == max(area_lev)])
}

# Survey data preprocessing:
set.seed(1234)

# Format circumcision data with age data reformatted
survey_circumcision <- survey_circumcision %>%
  # allocate random number from 0-5 for 95 entries
  dplyr::mutate(
    circ_age = case_when(
      circ_age == 95 ~ sample(seq(0, 5), n(), replace = TRUE),
      TRUE               ~ circ_age),
    # Correct status for those who have information on
    # circumcision but are missing circumcision status
    circ_status = ifelse(is.na(circ_status) & !is.na(circ_age), 1,
                  ifelse(is.na(circ_status) & !is.na(circ_age), 1,
                         ifelse(is.na(circ_status) & !is.na(circ_where), 1, circ_status)))
  ) %>%
  # merging individual weights to:
  # Merging on individual information to  the circumcision dataset
  left_join(
    (survey_individuals %>%
       dplyr::select(contains("id"), "sex", "age", "indweight"))
  ) %>%
  # Merging on cluster information to  the circumcision dataset
  left_join(
    (survey_clusters %>%
       dplyr::select(contains("id"), - survey_region_id) %>%
       distinct())
  ) %>%
  # Remove those with missing circumcision status
  filter(!is.na(circ_status), !is.na(age), !is.na(geoloc_area_id)) %>%
  # Adding age group and type of circumcision
  dplyr::mutate(
    agegr = as.numeric(cut(age,
                           breaks = c(seq(0, 65, by = 5), Inf),
                           labels = 1:14,
                           right = FALSE,
                           include.lowest = TRUE)),
    year = as.numeric(substr(survey_id, 4, 7)),
    # setting survey types
    type = case_when(
      # circ_who ==  "Healthcare worker"         ~ "MMC",
      tolower(circ_who) ==  "medical"          ~ "MMC",
      tolower(circ_where) == "medical"         ~ "MMC",
      # circ_who == "Traditional practioner"     ~ "TMC",
      tolower(circ_who) == "traditional"       ~ "TMC",
      tolower(circ_where) == "traditional"     ~ "TMC",
      circ_status != 0                                ~ "Missing",
      TRUE                                     ~ NA_character_
    )
  ) %>%
  # Altering column names
  dplyr::rename(area_id = geoloc_area_id)

# add area id"s:
# find last area columns
last_area_id <- last(names(areas_wide)[names(areas_wide) %like% "area_id" &
                                         nchar(names(areas_wide)) > 7])
highest_area_hierarchy <-  substr(last_area_id,
                                  nchar(last_area_id), nchar(last_area_id))
# areas_wide <- areas_wide %>%
#  dplyr::select(contains("area_"), -c(area_id))

survey_circumcision <- survey_circumcision %>%
  # add all area data up to last area hierarchy (but then select only required)
  left_join(areas_wide, by = "area_id") %>%
  mutate(
    area_id = eval(parse(text = paste0("area_id", highest_area_hierarchy)))
  ) %>%
  # Remove other area columns but keep area level of interest
  dplyr::select(
    -names(areas_wide)[!names(areas_wide) %in% "area_id"]
  )

# Removing unnecessary datasets
rm(survey_clusters, survey_individuals)

###########################
### Aggregating results ###
###########################

# Appending together
# collect results for lower area hierarchies by joining higher area
# hierarchies (do so until you reach "area 0")

area_lev_spec <- max(as.numeric(substr(survey_circumcision$area_id, 5, 5)),
                     na.rm = T)
# all area levels in the data (0 indexed)
area_levs <- seq_len(area_lev_spec) - 1

# join in area names
if (!"area_name" %in% names(survey_circumcision)) {
  survey_circumcision <- left_join(
    survey_circumcision,
    (areas %>%
       st_drop_geometry() %>%
       dplyr::select(area_id, area_name))
  )
}

# collect results for lower area hierarchies by joining higher area
# hierarchies (do so until you reach "area 0")
if (area_levs[1] > -1) {
  results_list <- lapply(area_levs, function(x) {
    add_area_id(
      df = (survey_circumcision %>% dplyr::select(-area_name)),
      df_areas_wide = areas_wide,
      par = list("area_lev" = area_lev_spec,
                 "area_lev_select" = x),
      add_keep_cols = c("circ_status", "indweight"))
  })
  # also add highest area hierarchy data
  results_list[[length(results_list) + 1]] <- survey_circumcision %>%
    dplyr::select(area_id, area_name, year, age, type, circ_status, indweight)
} else {
  results_list <- survey_circumcision %>%
    dplyr::select(area_id, area_name, year, age, type, circ_status, indweight)
}

# Appending together
survey_circumcision <- as.data.frame(rbindlist(results_list))

# remove unnecessary data
rm(results_list); gc()

################################################
###### Getting Survey points - all types #######
################################################

# types list
types <- list("Total" = c(unique(survey_circumcision$type)),
              "Medical" = "MMC",
              "Traditional" = "TMC")
# age groups
age_groups <- c("0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "54-59",
                "60-64", "65+",   "0+",    "10+",   "15+",   "15-24",
                "15-29", "15-39", "15-49", "10-29", "10-39", "10-49",
                "10-24")

# survey years
survey_years <- unique(survey_circumcision$year)

# loop through each type
results_surv <- lapply(seq_along(types), function(j) {
  
  # loop for each age group in the data (for each type)
  results_surv_type <- lapply(seq_along(age_groups), function(i) {
    
    if (grepl("-", age_groups[i]) == TRUE) {
      age1 <- as.numeric(strsplit(age_groups[i], "-")[[1]][1])
      age2 <- as.numeric(strsplit(age_groups[i], "-")[[1]][2])
    }
    # If no upper limit use this split
    if (grepl("\\+", age_groups[i]) == TRUE) {
      age1 <-  as.numeric(strsplit(age_groups[i], "+")[[1]][1])
      age2 <- Inf
    }
    # Getting proportions
    tmp <- survey_circumcision %>%
      filter(age >= age1, age <= age2) %>%
      group_by(area_id, year) %>%
      dplyr::summarise(
        Y_ind =  length(circ_status) *
          sum((circ_status == 1 & type %in% types[[j]]) * indweight, na.rm = TRUE) /
          sum(indweight, na.rm = TRUE),
        Y_obs = sum(circ_status == 1 & type %in% types[[j]]),
        N = length(circ_status),
        p_ind = Y_ind / N,
        p_obs = Y_obs / N,
        .groups = "drop"
      ) %>%
      # adding age group
      mutate(age_group = age_groups[i])
    return(tmp)
  })
  # append together results for each age group
  results_surv_type <- as.data.frame(
    rbindlist(results_surv_type, use.names = T)
  )
  
  # Adding to skeleton dataset and adding regional information
  results_surv_type <- expand.grid(
    area_id = sort(unique(results_surv_type$area_id)),
    year = survey_years,
    type = names(types)[j],
    age_group = age_groups
  ) %>%
    left_join(results_surv_type) %>%
    # Adding region information
    left_join(
      (areas %>%
         st_drop_geometry() %>%
         dplyr::select(contains("area"), -area_level_label)),
      by = "area_id"
    ) %>%
    left_join(
      (areas %>%
         st_drop_geometry() %>%
         dplyr::select(parent_area_id = area_id, parent_area_name = area_name)),
      by = "parent_area_id"
    )
  return(results_surv_type)
})
# append results
results_surv <- as.data.frame(rbindlist(results_surv))

####################
### Saving files ###
####################

# Saving files
fwrite(results_surv, save_loc)