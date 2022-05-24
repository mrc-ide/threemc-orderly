library(dplyr)
library(tidyr)
library(stringr)
library(haven)

# source("extract_funs.R")
source("src/00b_create_mics_survey_data/extract_funs.R")

# load areas
areas <- sf::st_drop_geometry(sf::read_sf("global/areas.geojson"))

# dataframes for recoding surveys
variable_recode = readxl::read_excel("../circumcision-coverage/raw/Survey extract/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA")
value_recode = readxl::read_excel("../circumcision-coverage/raw/Survey extract/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA")

#### MICS surveys ####

# Oli: I haven't included the code for how I found these surveys. Should this 
# be stored elsewhere? Along with Jeff's rdhs::search_variable_label code
mics_surveys_with_circ <- sort(c(
  "ZWE2014MICS", "GHA2017MICS", "BEN2014MICS", "SWZ2014MICS", "MWI2013MICS", 
  "GMB2018MICS", "SWZ2010MICS", "NGA2016MICS", "TCD2019MICS"
))

# setup connection to MICS data on sharepoint
sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = Sys.getenv("MICS_ORDERLY_PATH"))

# only pull through desired mics surveys
mics_sharepoint_df <- folder$list() %>%
  dplyr::filter(
    stringr::str_detect(
      name, paste0(tolower(mics_surveys_with_circ), collapse = "|")
    )
  )

mics_paths <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), Sys.getenv("MICS_ORDERLY_PATH"), mics_sharepoint_df$name)
# order alphabetically
mics_paths <- mics_paths[order(basename(mics_paths))]

# check that mics_surveys_with_circ and mics_paths have same order
toupper(stringr::str_remove(basename(mics_paths), ".rds")) == mics_surveys_with_circ

mics_files <- lapply(mics_paths, spud::sharepoint_download, sharepoint_url = Sys.getenv("SHAREPOINT_URL"))

# There needs to be some additional code to rename the datasets themselves when 
# they are non-standard in the MICS files. 
# e.g. using similar logic to renaming variables. This excel segment is 
# currently in the "variable recode" tab, but strictly these are not variables. 
# Should be moved to a new tab I think.
# 
# _default_mics	dataset_rename	woman_dataset	          wm
# _default_mics	dataset_rename	household_dataset	      hh
# _default_mics	dataset_rename	birth_dataset	          bh
# SWZ2000MICS	  dataset_rename	woman_dataset	          wmsw
# CIV2000MICS	  dataset_rename	woman_dataset	          CIwm
# CMR2000MICS	  dataset_rename	woman_dataset	          wmca
# SWZ2000MICS	  dataset_rename	household_dataset	      hhsw
# CIV2000MICS	  dataset_rename	household_dataset	      CIhh
# CMR2000MICS	  dataset_rename	household_dataset	      hhca
 
# Currently surveys with custom dataset names are not extracted

# read in surveys for adult men
mics_dat <- lapply(mics_files, readRDS) %>%
  lapply("[", "mn") %>% 
  unlist(recursive = FALSE) %>%
  setNames(mics_surveys_with_circ) %>% 
  lapply(function(x) {
    x %>% `colnames<-`(tolower(names(x)))
  })

# circ_raw <- c(circ_raw, mics_dat)
circ_raw <- mics_dat

mics_file_type <- setNames(rep("mn", length(mics_dat)), names(mics_dat))

#### Extract and recode variables ####

file_type <- c(
  # c("Individual Recode" = "ir", "Men's Recode" = "mr")[combined_datasets$FileType] %>% setNames(combined_datasets$survey_id)
  mics_file_type # ,
  # phia_file_type
)

# dataset with codes for mics_area_name 
mics_indicators <- readr::read_csv("global/MICS_indicators.csv") %>% 
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

# join both recoding dfs
variable_recode <- variable_recode %>% 
  bind_rows(
    (mics_indicators %>% 
       # change mics_indicator colnames to match variable_recode convention
       rename(
         # dataset = id, variable = filetype, 
         dataset = filetype, variable = id, 
         var_raw = value, var_label_raw = label
       ) %>% 
       mutate(analysis = "circ", dataset = "mn") %>% # valid??
       # don't have female date of births or interviews
       filter(!var_raw %in% c("wdoi", "wdob")))
) %>% 
  # add in default row for mics_areas (hh7) for dataset == mn
  # also add rows for date of birth and date of interview (two different forms)
  bind_rows(
    tibble::tribble(
      ~survey_id,      ~dataset, ~variable,        ~var_raw,  ~var_label_raw,                ~analysis,
      "_default_mics", "mn",     "mics_area_name", "hh7",     "MICS area level",             "circ",
      "_default_mics", "mn",     "dob",            "mwdob",   "Date of birth of man (CMC)",  "circ",
      "_default_mics", "mn",     "dob",            "mdob",    "Date of birth of man (CMC)",  "circ",
      "_default_mics", "mn",     "doi",            "mwdoi",   "Date of interview men (CMC)", "circ",
      "_default_mics", "mn",     "doi",            "mdoi",    "Date of interview men (CMC)", "circ"
    )
  )

# extract survey variables, recode ID variables in in MICS surveys
circ_extracted <- Map(
  extract_survey_vars,
  df = circ_raw,
  survey_id = names(circ_raw),
  list(variable_recode),
  file_type[names(circ_raw)],
  analysis = "circ",
  # want to keep, but not recode 
  add_vars = list(c("mnweight")))

# pull labels for mics_area_name
labels <- lapply(circ_extracted, function(x) {
  labelled::val_labels(x$mics_area_name)
})

# create df for each coutry to be used as a key for replacing area val w/ name
labels_dfs <- lapply(labels, function(x) {
  data.frame(
    "mics_area_name" = x,
    "area_name" = names(x)
  )
})

# replace mics_area_name value with actual location!
circ_extracted <- lapply(seq_along(circ_extracted), function(i) {
  if ("mics_area_name" %in% names(circ_extracted[[i]])) {
    left_join(
      circ_extracted[[i]], labels_dfs[[i]], by = "mics_area_name" 
    ) %>% 
      select(-matches("mics_area_name"))
  } else return(circ_extracted[[i]])
  left_join(
    circ_extracted[[i]], labels_dfs[[i]], by = "mics_area_name" 
  ) %>% 
    select(-matches("mics_area_name"))
})
names(circ_extracted) <- names(circ_raw)

# OLI: Note on the value_recode tab of the excel file
# There are several cases where though the variable name is custom to the survey, 
# the value coding is the same as the default. 
# The value recode entries for those surveys can be removed, but for speed I 
# just added them all. I'm not sure removing them is any better than leaving 
# them in though. The size of the value recode book is immaterial. 
# circ_extracted_test <- circ_extracted[grepl("GHA", names(circ_extracted))]
# circ_extracted_test <- circ_extracted[grepl("NGA", names(circ_extracted))]
circ_recoded <- Map(
  recode_survey_variables,
  df = circ_extracted,
  survey_id = names(circ_extracted),
  list(value_recode),
  file_type[names(circ_extracted)],
  analysis = "circ"
)

# remove invalid surveys
mics_final <- circ_recoded %>%
  lapply(function(x) {
    if(ncol(x) < 6) {
      NULL
    } else if (length(unique(x$circ_status)) == 1 && is.na(unique(x$circ_status))) {
      NULL
    } else {
      x
    }
  }) %>%
  # remove empty list elements (handy!) and join data
  purrr::compact() %>% 
  bind_rows()

# add in remaining columns and fix "missed" recoding in circ_status
mics_final <- mics_final %>% 
  mutate(
    iso3 = substr(survey_id, 1, 3),
    sex  = "male", # can be assumed
    # convert CMC -> POSIXct (& Ethiopian to Gregorian Calendar)
    dob = ifelse(
      iso3 == "ETH", 
      cmc_as_date(ethiopian_to_greg(dob)), 
      cmc_as_date(dob)
    ),
    doi = ifelse(
      iso3 == "ETH", cmc_as_date(ethiopian_to_greg(doi)), cmc_as_date(doi)
    ),
    # age at interview = floor(date of interview - date of birth)
    age = floor(lubridate::interval(dob, doi) / lubridate::years(1)),
    # fix circ_status which aren't recoded (seem to have "raw_val" not "val")
    circ_status = case_when(
      circ_status == 2 ~ 0L,          # raw value of 2 corresponds to 0 => No
      circ_status == 9 ~ NA_integer_, # "" "" "Missing" => NA
      TRUE             ~ circ_status
    ),
    # fix issues with area names (change to match areas sf file)
    area_name = case_when(
      # capitalise the word "city", as it is in areas
      grepl("city", tolower(area_name)) ~ stringr::str_to_title(area_name),
      # GHA: Area names shouldn't be capitalised
      iso3 == "GHA" ~ stringr::str_to_title(area_name),
      # BEN: fix region with non-ascii letters
      area_name == "OuÃ©mÃ©" ~ "Oueme",
      # GMB: regions appear to be joined in areas, change to match
      iso3 == "GMB" & grepl("Banjul|Kanifing|Brikama", area_name) ~ "Banjul/Kanifing/Brikama",
      iso3 == "GMB" & grepl("Kuntaur|Janjanbureh", area_name) ~ "Kuntaur/Janjanbureh",
      # TCD: Some strange parsing errors for accents & lack of hyphons
      iso3 == "TCD" & area_name == "Guera" ~ "GuÃ©ra",
      iso3 == "TCD" & area_name == "Tandjile" ~ "TandjilÃ©",
      iso3 == "TCD" & area_name == "Ndjamena" ~ "N'Djamena",
      iso3 == "TCD" & area_name == "Hadjer Lamis" ~ "Hadjer-Lamis",   
      iso3 == "TCD" & area_name == "Mayo Kebbi Est" ~ "Mayo-Kebbi Est",
      iso3 == "TCD" & area_name == "Mayo Kebbi Ouest" ~ "Mayo-Kebbi Ouest",
      iso3 == "TCD" & area_name == "Barh El Gazal" ~ "Barh-El-Gazel",  
      iso3 == "TCD" & area_name == "Chari Baguirmi" ~ "Chari-Baguirmi",  
      iso3 == "TCD" & area_name == "Moyen Chari" ~ "Moyen-Chari",  
      # GHA: FCT Abuja is just Abuja
      iso3 == "GHA" & area_name == "FCT Abuja" ~ "FCT",
      TRUE ~ area_name
    )
  ) %>% 
  select(-c(dob, doi)) %>% 
  # rename weights column to match circumcision colnames
  rename(indweight = mnweight)

# check that any survey ages are greater than circumcision ages
# something wrong with circ_age! 8 times where circ_age > surveyed age
mics_final %>% 
  filter(circ_age > age) %>% 
  nrow()

# add area_id to dataset
areas_join <- areas %>% 
  # # hh7 (i.e. area_name) maps to area lev 1, filter to avoid duplication
  filter(
    (area_level == 1) |
    # special cases
    (iso3 == "MWI" & area_level %in% c(3, 5)) |  # need lev 3 for Mzimba
    (iso3 == "NGA" & area_level == 2)
  ) %>% 
  select(area_id, area_name)

# split by iso3 before joining with areas_join, again to avoid duplication 
mics_final_list <- split(mics_final, mics_final$iso3)
mics_final <- bind_rows(lapply(seq_along(mics_final_list), function(i) {
  mics_final_list[[i]] %>% 
    left_join(
      filter(areas_join, grepl(names(mics_final_list)[i], area_id)), 
      by = "area_name"
    )
}))

# check that no duplication has occurred
mics_final %>% 
  mutate(
    check = paste(survey_id, individual_id, cluster_id, household, line, circ_status, circ_age, circ_where, area_name, iso3, sex)
  ) %>% 
  filter(duplicated(check)) %>% 
  nrow() == 0

# finally, assign any surveys with NA for area_id to the national level
mics_final <- mics_final %>% 
  mutate(
    across(contains("area"), ~ ifelse(is.na(area_id), iso3, .)),
    area_name_test = ifelse(
      area_name == iso3, 
      countrycode(area_name, origin = "iso3c", destination = "country.name"), 
      area_name
    )  
)

# save
readr::write_csv(mics_final, file = "~/imperial_repos/circumcision-coverage/data/mics_surveys.csv.gz")
readr::write_csv(mics_final, file = "~/imperial_repos/threemc-orderly/global/mics_surveys.csv.gz")

survey_circumcision <- readr::read_csv("global/survey_circumcision_no_mics.csv.gz")
survey_clusters <- readr::read_csv("global/survey_clusters.csv.gz")
survey_individuals <- readr::read_csv("global/survey_individuals.csv.gz")

# start of script from survey_circumcision
survey_circumcision <- survey_circumcision %>%
  # Merging on individual information to  the circumcision dataset
  dplyr::left_join(
    survey_individuals %>%
      dplyr::select(.data$survey_id, .data$cluster_id, .data$individual_id,
                    .data$sex, .data$age, .data$indweight),
    by = c("survey_id", "individual_id")
  ) %>%
  # Merging on cluster information to the circumcision dataset
  dplyr::left_join(
    (survey_clusters %>%
       dplyr::mutate(area_id = as.character(.data$geoloc_area_id)) %>% 
       dplyr::select(.data$survey_id, .data$cluster_id, .data$area_id)),
    by = c("survey_id", "cluster_id")
  )

# check that all survey_circumcision columns are contained in mics data
names(survey_circumcision)[!names(survey_circumcision) %in% names(mics_final)]

survey_circumcision <- mics_final %>%
  # ensure cluster_id isn't coerced from character upon joining
  mutate(cluster_id = as.character(cluster_id)) %>% 
  bind_rows(survey_circumcision) %>% 
  # arrange as before
  arrange(iso3, survey_id, age, circ_age)

# Check that each MICS survey has valid circ_status and area_id values
valid_surveys <- mics_final %>% 
  filter(!is.na(circ_status) & !is.na(area_id) & !is.na(indweight)) %>% 
  distinct(survey_id) %>% 
  pull()
(invalid_surveys <- mics_surveys_with_circ[!mics_surveys_with_circ %in% valid_surveys])

# areas with NA for area_id (should be none)
mics_final %>% 
  filter(is.na(area_id)) %>% 
  distinct(iso3, area_name, area_id)

# save survey_circumcision
readr::write_csv(survey_circumcision, file = "~/imperial_repos/circumcision-coverage/data/survey_circumcision.csv.gz")
