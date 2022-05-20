library(tidyverse)
library(rdhs) # api to DHS data

# source("extract_funs.R")
source("")

ssa_iso <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

variable_recode = readxl::read_excel("~/Imperial College London/HIV Inference Group - WP - Documents/Circumcision coverage/raw/Survey extract/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA")
value_recode = readxl::read_excel("~/Imperial College London/HIV Inference Group - WP - Documents/Circumcision coverage/raw/Survey extract/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA")

dhs_survey_characteristics() %>%
  filter(grepl("circumcision", SurveyCharacteristicName))

survey_has_circ <- dhs_surveys(surveyCharacteristicIds = 59) %>%
  filter(!SurveyId %in% c("LB2019DHS", "GN2012DHS")) %>%
  mutate(survey_id = paste0(
    dhscc_to_iso3(DHS_CountryCode),
    SurveyYear,
    SurveyType
  ))

#' Men's recode datasets
mrd <- dhs_datasets(fileType = "MR", fileFormat = "FL")

combined_datasets %>% filter(CountryName == "Mozambique")

#' Get Individual recode datasets with circumcision characteristic and bind in MR datasets
combined_datasets <- dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  filter(SurveyId %in% dhs_surveys(surveyCharacteristicId = 11)$SurveyId) %>%
  filter(!SurveyId %in% mrd$SurveyId) %>%
  bind_rows(mrd
            # filter(SurveyId %in% survey_has_circ$SurveyId)
  ) %>%
  filter(dhscc_to_iso3(DHS_CountryCode) %in% ssa_iso,
         as.integer(SurveyYear) > 1999) %>%
  mutate(survey_id = paste0(
    dhscc_to_iso3(DHS_CountryCode),
    SurveyYear,
    SurveyType
  )) %>%
  filter(!survey_id %in% c("LSO2014DHS") # Jeff: Variables for both medical and traditional
  )
#' 
#' #' Jeff: Required to get around rdhs cache bug - as you will have different surveys, highly likely that you will encounter surveys that are not in the variable codebook and will be extracted/recoded incorrectly.
#' #' The code will probably crash as a result. 
#' 
#' dl <- rdhs::get_downloaded_datasets()
#' dl <- names(dl)
#' dl <- paste0(dl, ".ZIP")
#' 
#' #' For lack of a better solution - read in my downloaded survey list to replicate what I've had access to..
#' 
#' dl <- read.csv("rdhs_bug_dl.csv")[,1]
#' 
#' combined_datasets <- combined_datasets %>%
#'   filter(FileName %in% dl)

circ_raw <- get_datasets(combined_datasets, clear_cache = TRUE) %>%
  setNames(combined_datasets$survey_id) %>%
  .[grepl("\\.rds$", .)] %>%
  lapply(readRDS)

#### PHIA surveys
tmp <- tempdir()

phia_paths <- lapply(list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/household surveys/PHIA/datasets", full.names = TRUE), list.files, full.names=TRUE, pattern = "dataset") %>%
  lapply(list.files, full.names = TRUE, pattern = "Interview") %>%
  lapply(grep, pattern = "CSV).zip", value =TRUE) %>%
  lapply(grep, pattern = "Child", value =TRUE, invert=TRUE) %>%
  unlist

lapply(phia_paths, function(x) {
  message(x)
  unzip(x, exdir = tmp)
})

phia_files <- list.files(tmp, full.names = TRUE)
phia_path <- grep("adultind", phia_files, value=TRUE) ## Only finding 11 PHIA paths from 12. CAMPHIA is in a nested folder
phia_path <- c(phia_path, file.path(tmp, "203 CAMPHIA 2017-2018 Adult Interview Dataset (CSV)/camphia2017adultind.csv"))

phia_dat <- lapply(phia_path, function(x) {
  dat <- read.csv(x)
  dat %>%
    mutate(across(everything(), str_trim),
           across(everything(), str_replace_all, pattern = "\\.", replacement = NA_character_)) %>%
    type.convert()
})

# lapply(dat, function(dat) {
#   grep(x = colnames(dat), pattern = "mc", value=TRUE)
# })

names(phia_dat) <- c(
  "CIV2017PHIA",
  "ETH2017PHIA",
  "LSO2016PHIA",
  "MWI2015PHIA",
  "NAM2017PHIA",
  "RWA2018PHIA",
  "SWZ2016PHIA",
  "TZA2016PHIA",
  "UGA2016PHIA",
  "ZWE2015PHIA",
  "ZMB2016PHIA",
  "CMR2017PHIA"
)

circ_raw <- c(circ_raw, phia_dat)

phia_file_type <- rep("phia", length(phia_dat)) %>% setNames(names(phia_dat))

### MICS surveys

## I haven't included the code for how I found these surveys. Should this be stored elsewhere? Along with Jeff's rdhs::search_variable_label code
mics_surveys_with_circ <- c("ZWE2014MICS", "GHA2017MICS", "BEN2014MICS", "SWZ2014MICS", "MWI2013MICS", "GMB2018MICS", "SWZ2010MICS", "NGA2016MICS", "TCD2019MICS")

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = Sys.getenv("MICS_ORDERLY_PATH"))

mics_sharepoint_df <- folder$list() %>%
  dplyr::filter(str_detect(name, paste0(tolower(mics_surveys_with_circ), collapse = "|")))

mics_paths <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), Sys.getenv("MICS_ORDERLY_PATH"), mics_sharepoint_df$name)
mics_files <- lapply(mics_paths, spud::sharepoint_download, sharepoint_url = Sys.getenv("SHAREPOINT_URL"))

#' There needs to be some additional code to rename the datasets themselves when they are non-standard in the MICS files. 
#' e.g. using similar logic to renaming variables. This excel segment is currently in the "variable recode" tab, but strictly these are not variables. Should be moved to a new tab I think.
#' 
#' _default_mics	dataset_rename	woman_dataset	          wm
#' _default_mics	dataset_rename	household_dataset	      hh
#' _default_mics	dataset_rename	birth_dataset	          bh
#' SWZ2000MICS	  dataset_rename	woman_dataset	          wmsw
#' CIV2000MICS	  dataset_rename	woman_dataset	          CIwm
#' CMR2000MICS	  dataset_rename	woman_dataset	          wmca
#' SWZ2000MICS	  dataset_rename	household_dataset	      hhsw
#' CIV2000MICS	  dataset_rename	household_dataset	      CIhh
#' CMR2000MICS	  dataset_rename	household_dataset	      hhca
#' 
#' Currently surveys with custom dataset names are not extracted

mics_dat <- lapply(mics_files, readRDS) %>%
  lapply("[", "mn") %>%
  unlist(recursive = FALSE) %>%
  setNames(mics_surveys_with_circ) %>%
  lapply(function(x) {
    x %>%
      `colnames<-`(tolower(names(x)))
  })

circ_raw <- c(circ_raw, mics_dat)

mics_file_type <- rep("mn", length(mics_dat)) %>% setNames(names(mics_dat))

### Extract and recode variables

file_type <- c(
  c("Individual Recode" = "ir", "Men's Recode" = "mr")[combined_datasets$FileType] %>% setNames(combined_datasets$survey_id)
  # mics_file_type,
  # phia_file_type
)

circ_extracted <- circ_raw %>%
  Map(extract_survey_vars,
      df = .,
      survey_id = names(.),
      list(variable_recode),
      file_type[names(.)],
      analysis = "circ")

#' Note on the value_recode tab of the excel file
#' There are several cases where though the variable name is custom to the survey, the value coding is the same as the default. 
#' The value recode entries for those surveys can be removed, but for speed I just added them all. I'm not sure removing them is any better than leaving them in though. The size of the value recode book is immaterial. 
circ_recoded <- circ_extracted  %>%
  Map(recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode),
      file_type[names(.)],
      analysis = "circ"
  )

foo <- circ_recoded %>%
  lapply(function(x) {
    if(ncol(x) <6)
      NULL
    else if(length(unique(x$circ_status)) == 1 & is.na(unique(x$circ_status)))
      NULL
    else
      x
  }) %>%
  compact()

saveRDS(foo, "~/Imperial College London/HIV Inference Group - WP - Documents/Circumcision coverage/raw/Survey extract/circ_recoded_dhs.rds")

debugonce(extract_survey_vars)
foo <- extract_survey_vars(circ_raw$SEN2005DHS, "SEN2005DHS", variable_recode, "mr", "circ")