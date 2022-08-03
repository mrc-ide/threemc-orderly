
# ensure save_dir exists
save_dir <- "artefacts/"
threemc::create_dirs_r(save_dir)

# load areas
areas <- sf::st_drop_geometry(sf::read_sf("depends/areas.geojson"))

# Path to sharepoint directory
sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

# function to load data from specific dir on sharepoint
load_sharepoint_data <- function(
                                 path, pattern = NULL, group = "HIVInferenceGroup-WP/"
                                 ) {

  # List files in folder
  folder <- sharepoint$folder(group, URLencode(path))

  # pull urls for each file
  urls <- URLencode(file.path("sites", group, path, folder$files()$name))

  # may only require certain files
  if (!is.null(pattern)) {
    # only want cluster, individuals and circumcision data
    urls <- urls[grepl(pattern, urls)]
  }

  # download files, name with urls so we know order of temp files
  files = lapply(urls, sharepoint$download)
  if (length(files) == 0) stop("No files found at supplied path")
  names(files) <- basename(urls)

  return(files)
}

# load dataframes for recoding surveys from sharepoint
recode_xlsx <- load_sharepoint_data(
  path = "Shared Documents/Circumcision Coverage/raw/Survey extract/",
  pattern = "hivdata_survey_datasets.xlsx"
)
variable_recode <- readxl::read_excel(
  recode_xlsx$hivdata_survey_datasets.xlsx,
  sheet = "variable_recode",
  na = "NA"
)
value_recode <- readxl::read_excel(
  recode_xlsx$hivdata_survey_datasets.xlsx,
  sheet = "value_recode",
  na = "NA"
)


#### MICS surveys ####

#' CAF2006MICS survey:
#' * Male questionnaire includes question "Are you circumcised?" (HHA19)
#' * However, an odd skip pattern: only asked of men who:
#'   - (1) respond 'yes' to "Have you ever heard of AIDS?" (HHA1A), AND
#'   - (2) respond 'no' to "Have you ever been tested for AIDS?" (HHA17A)
#' -> EXCLUDE CAF2006MICS for now

# Oli: I haven't included the code for how I found these surveys. Should this
# be stored elsewhere? Along with Jeff's rdhs::search_variable_label code
mics_surveys_with_circ <- c("BEN2014MICS",
                            ## "CAF2006MICS",
                            "CAF2018MICS",
                            "GHA2017MICS",
                            "GMB2018MICS",
                            "GNB2014MICS",
                            "GNB2018MICS",
                            "MWI2013MICS",
                            "MWI2019MICS",
                            "NGA2016MICS",
                            ## "STP2019MICS",  # no boundaries
                            "SWZ2010MICS",
                            "SWZ2014MICS",
                            "TCD2019MICS",
                            "ZWE2014MICS")


# load mics surveys with circumcision data from sharepoint
mics_paths <- load_sharepoint_data(
  path = Sys.getenv("MICS_ORDERLY_PATH"),
  pattern = paste(tolower(mics_surveys_with_circ), collapse = "|")
)
# order alphabetically
mics_paths <- mics_paths[order(names(mics_paths))]

# check that mics_surveys_with_circ and mics_paths have same order
stopifnot(
  mics_surveys_with_circ == toupper(stringr::str_remove(
    names(mics_paths), ".rds"
  ))
)

# There needs to be some additional code to rename the datasets themselves when
# they are non-standard in the MICS files.
# e.g. using similar logic to renaming variables. This excel segment is
# currently in the "variable recode" tab, but strictly these are not variables.
# Should be moved to a new tab I think.
#
# _default_mics dataset_rename  woman_dataset             wm
# _default_mics dataset_rename  household_dataset             hh
# _default_mics dataset_rename  birth_dataset             bh
# SWZ2000MICS     dataset_rename        woman_dataset             wmsw
# CIV2000MICS     dataset_rename        woman_dataset             CIwm
# CMR2000MICS     dataset_rename        woman_dataset             wmca
# SWZ2000MICS     dataset_rename        household_dataset             hhsw
# CIV2000MICS     dataset_rename        household_dataset             CIhh
# CMR2000MICS     dataset_rename        household_dataset             hhca

# TEMPORARY: code below specifies custom dataset names

mics_male_dataset <- setNames(rep("mn", length(mics_surveys_with_circ)), mics_surveys_with_circ)
## mics_male_dataset[["CAF2006MICS"]] <- "mm"

# read in surveys for adult men
mics_dat <- lapply(mics_paths, readRDS) %>%
  Map("[", ., mics_male_dataset) %>%
  unlist(recursive = FALSE) %>%
  setNames(mics_surveys_with_circ) %>%
  lapply(function(x) {
    x %>% `colnames<-`(tolower(names(x)))
  })

# circ_raw <- c(circ_raw, mics_dat)
circ_raw <- mics_dat


#### Extract and recode variables ####


# dataset with codes for mics_area_name
mics_indicators <- readr::read_csv("depends/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

#' Add MWI2019MICS custom area_level
mics_indicators <- mics_indicators %>%
  bind_rows(
    tribble(
      ~label,            ~id,            ~filetype, ~survey_id, ~value,
      "MICS area name",  "mics_area_name", "hh", "MWI2019MICS", "stratum",
      "MICS area level", "mics_area_level", NA,  "MWI2019MICS", "5"
    )
  )


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
      "SWZ2010MICS",   "mn",     "dob",            "mdob",    "Date of birth of man (CMC)",  "circ",
      "_default_mics", "mn",     "dob",            "mwdob",   "Date of birth of man (CMC)",  "circ",
      "SWZ2010MICS",   "mn",     "doi",            "mdoi",    "Date of interview men (CMC)", "circ",
      "_default_mics", "mn",     "doi",            "mwdoi",   "Date of interview men (CMC)", "circ",
    )
  )

#' CAF2006MICS adjustments:
#' * Dataset is "mm" (instead of "mn")
#' * DOB variable is `wdob`
#' * circ_status is `hha19`

variable_recode <- variable_recode %>%
  mutate(dataset = if_else(survey_id == "CAF2006MICS" & analysis == "circ", "mm", dataset)) %>%
    bind_rows(
    tibble::tribble(
      ~survey_id,      ~dataset, ~variable,        ~var_raw,  ~var_label_raw,                ~analysis,
      "CAF2006MICS",   "mm",     "circ_status",    "hha19",   "Etes vous circoncis",         "circ",
      "CAF2006MICS",   "mm",     "dob",            "wdob",    "Date of birth (CMC)",         "circ",
      "CAF2006MICS",   "mm",     "mnweight",       "meweight", "[Sampling weight]",          "circ",
      "CAF2006MICS",   "mm",     "cluster_id",     "hh1",     "Cluster ID - man dataset",      NA,
      "CAF2006MICS",   "mm",     "household",      "hh2",     "Household ID - man dataset",    NA,
      "CAF2006MICS",   "mm",     "line",           "ln",      "Line number - man dataset",     NA
      )
  )



# extract survey variables, recode ID variables in in MICS surveys
circ_extracted <- Map(
  extract_survey_vars,
  df = circ_raw,
  survey_id_c = names(circ_raw),
  variable_recode = list(variable_recode),
  dataset_type = mics_male_dataset[names(circ_raw)],
  analysis_c = "circ",
  # want to keep, but not recode
  add_vars = list(c("mnweight", "meweight", "mwm6y", "mwm6m", "mwm6d"))
)

#' Rename CAF2006MICS weight variable to be consistent with other surveys
#' !!! NOTE JE: I'm unclear why weight variable is handled like this

## circ_extracted$CAF2006MICS <- circ_extracted$CAF2006MICS %>%
##   rename(mnweight = meweight)

#' NGA2016MICS recorded date of interview as Y-M-D (no CMC DOI variable).
#' -> Manually recode as doi

circ_extracted$NGA2016MICS <- circ_extracted$NGA2016MICS %>%
  mutate(
    doi = 12 * (mwm6y - 1900) + as.integer(mwm6m)
  )
    
# pull labels for mics_area_name
labels <- lapply(circ_extracted, function(x) {
  labelled::val_labels(x$mics_area_name)
})

# create df for each country to be used as a key for replacing area val w/ name
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
# There are several cases where though the var name is custom to the survey,
# the value coding is the same as the default.
# The value recode entries for those surveys can be removed, but for speed I
# just added them all. I'm not sure removing them is any better than leaving
# them in though. The size of the value recode book is immaterial.

#' JE edit: add value recode for CAF2006MICS in mm dataset

value_recode <- value_recode %>%
  bind_rows(
    tribble(
      ~survey_id,    ~dataset, ~variable,     ~val_raw, ~val_label_raw, ~value, ~notes, ~var_raw, ~analysis,
      "CAF2006MICS", "mm",     "circ_status", "1",      "Oui",          "1",    NA,     "hha19",  "circ",
      "CAF2006MICS", "mm",     "circ_status", "2",      "Non",          "0",    NA,     "hha19",  "circ",
      "CAF2006MICS", "mm",     "circ_status", "9",      "Manquant",     NA,     NA,     "hha19",  "circ"
    )
  )

circ_recoded <- Map(
  recode_survey_variables,
  df = circ_extracted,
  survey_id = names(circ_extracted),
  list(value_recode),
  mics_male_dataset[names(circ_extracted)],
  analysis = "circ"
)

# remove invalid surveys
mics_final <- circ_recoded %>%
  lapply(function(x) {
    if(ncol(x) < 6) {
      NULL
    } else if (
      length(unique(x$circ_status)) == 1 && is.na(unique(x$circ_status))
    ) {
      NULL
    } else {
      x
    }
  }) %>%
  # remove empty list elements (handy!) and join data
  purrr::compact() %>%
  bind_rows() %>%
  select(-mwm6y, -mwm6m, -mwm6d)


#' Soft validation: check <25% missing for expected complete variables
#' This will ensure that variable not coded entirely as NA (unexpectedly
#' missed during extract)

check_not_all_na <- function(var, threshold = 0.25) {
  mics_final %>%
    group_by(survey_id) %>%
    summarise(mean(is.na({{var}})) <= threshold) %>%
    pull() %>%
    stopifnot()
}

check_not_all_na(dob)
check_not_all_na(doi)
check_not_all_na(area_name)
check_not_all_na(circ_status)
check_not_all_na(individual_id, 0.0)

#' Handful of missing area_name for TCD2019MICS, mnweight also =0
mics_final %>%
  filter(is.na(area_name))

mics_final <- mics_final %>%
  filter(! (mnweight == 0 & is.na(area_name) & survey_id == "TCD2019MICS"))

check_not_all_na(area_name, 0.0)

stopifnot(mics_final$circ_status %in% c(0, 1, NA))

# add in remaining columns and fix "missed" recoding in circ_status
mics_final <- mics_final %>%
  mutate(
    iso3 = substr(survey_id, 1, 3),
    sex  = "male", # can be assumed
    # age at interview = floor(date of interview - date of birth)
    age = floor( (doi - dob) / 12),
  ) %>%
  rename(
    indweight = mnweight,
    dob_cmc = dob,
    interview_cmc = doi
  )

#' Recode area_name to match areas file

mics_final <- mics_final %>%
  mutate(
    area_name = case_when(
      # capitalise the word "city", as it is in areas (only required for MWI)
      grepl("city", tolower(area_name)) ~ stringr::str_to_title(area_name),
      ## 
      ## GHA: Area names shouldn't be capitalised
      iso3 == "GHA"                     ~ stringr::str_to_title(area_name) %>%
        recode("Brong Ahafo" = "Ahafo"),
      ##
      ## BEN: fix region with non-ascii letters
      area_name == "OuÃ©mÃ©"            ~ "Oueme",
      ##
      ## GMB: regions appear to be joined in areas, change to match
      iso3 == "GMB" & grepl("Banjul|Kanifing|Brikama", area_name) ~ "Banjul/Kanifing/Brikama",
      iso3 == "GMB" & grepl("Kuntaur|Janjanbureh", area_name)     ~ "Kuntaur/Janjanbureh",
      ##
      ## TCD: Some strange parsing errors for accents & lack of hyphens
      iso3 == "TCD" & area_name == "Guera"            ~ "GuÃ©ra",
      iso3 == "TCD" & area_name == "Tandjile"         ~ "TandjilÃ©",
      iso3 == "TCD" & area_name == "Ndjamena"         ~ "N'Djamena",
      iso3 == "TCD" & area_name == "Hadjer Lamis"     ~ "Hadjer-Lamis",
      iso3 == "TCD" & area_name == "Mayo Kebbi Est"   ~ "Mayo-Kebbi Est",
      iso3 == "TCD" & area_name == "Mayo Kebbi Ouest" ~ "Mayo-Kebbi Ouest",
      iso3 == "TCD" & area_name == "Barh El Gazal"    ~ "Barh-El-Gazel",
      iso3 == "TCD" & area_name == "Chari Baguirmi"   ~ "Chari-Baguirmi",
      iso3 == "TCD" & area_name == "Moyen Chari"      ~ "Moyen-Chari",
      ##
      ## NGA: FCT Abuja is just Abuja
      iso3 == "NGA" & area_name == "FCT Abuja" ~ "FCT",
      ##
      ## CAF: Rename regions to 'R1', ..., 'R7'
      survey_id == "CAF2018MICS" ~ sub("RÃ©gion ([0-7])", "RS\\1", area_name),
      ##
      ## GNB
      iso3 == "GNB" ~ area_name %>%
        recode("Bolama/Bijagós" = "Bolama/Bijagos",
               "Bafatá" = "Bafata",
               "Gabú" = "Gabu",
               "SAB" = "Bissau",
               "Bolama/BijagÃ³s" = "Bolama/Bijagos",
               "BafatÃ¡" = "Bafata",
               "GabÃº" = "Gabu",          
               "SAB" = "Bissau"),
      ##
      ## MWI
      ## Note: keeping Mzuzu City as part of Mzimba district
      survey_id == "MWI2019MICS" & grepl("Blantyre|Lilongwe|Zomba", area_name) ~ sub("Urban", "City", area_name) %>%
        sub(" +Rural", "", .),
      survey_id == "MWI2019MICS"  ~ sub(" Urban| Rural", "", area_name) %>%
        recode("Nkhata Bay" = "Nkhatabay"),
      TRUE ~ area_name
    )
  )



# check that any survey ages are greater than circumcision ages
# something wrong with circ_age! 8 times where circ_age > surveyed age
# Let Oli know
n <- mics_final %>%
  filter(circ_age > age) %>%
  nrow()

print(n)

# if there are only a few surveys where this is the case, can be safely dropped
if (n > 0 && n < 20) {
  mics_final <- mics_final %>%
    filter(is.na(circ_age) | is.na(age) | circ_age <= age)
} else {
  stop("Too many errors in circ_age > age; something likely wrong")
}

# add area_id to dataset
areas_join <- areas %>%
  # # hh7 (i.e. area_name) maps to area lev 1, filter to avoid duplication
  filter(
    (area_level == 1 & !iso3 %in% c("MWI", "NGA")) |
      # special cases
      (iso3 == "MWI" & (area_name == "Mzimba" & area_level == 3 | area_level == 5)) |  # need lev 3 for Mzimba
      (iso3 == "NGA" & area_level == 2)
  ) %>%
  select(iso3, area_id, area_name, area_level) %>%
  arrange()

#' Confirm area_name is unique within iso3
areas_join %>%
  count(iso3, area_name) %>%
  pull(n) %>%
  {stopifnot(. == 1)}


mics_final <- mics_final %>%
  left_join(areas_join, by = c("iso3", "area_name"))

stopifnot(!is.na(mics_final$area_id))

# check that no duplication has occurred
mics_final %>%
  mutate(dup = !duplicated(paste(survey_id, individual_id))) %>%
  pull() %>%
  stopifnot()

#' Reorder columns

mics_final <- mics_final %>%
  select(iso3, survey_id, area_id, area_name, individual_id, cluster_id, household, line,
         sex, age, dob_cmc, interview_cmc, everything())

# save MICS surveys
readr::write_csv(
  mics_final,
  "artefacts/mics_surveys.csv.gz"
)
