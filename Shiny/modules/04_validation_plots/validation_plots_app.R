#### Libraries ####

library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyhelper)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(sf)
library(gridExtra)
library(geofacet)
library(scales)
library(ggridges)
library(orderly)
library(threemc)
# devtools::load_all("~/imperial_repos/threemc")

#### Data ####

# orderly root 
orderly_root <- here::here()
orderly_root <- unlist(stringr::str_split(orderly_root, "/"))
orderly_root <- orderly_root[1:which(orderly_root == "threemc-orderly")]
orderly_root <- paste(orderly_root, collapse = "/")

ssa_iso3 <- sort(c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
  "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN",
  "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA",
  "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
))

# temp use a few countries to test
# ssa_iso3 <- c("LSO")

# Original results for age groups 
archives <- orderly::orderly_list_archive()
dir_name <- orderly::orderly_search(
  query = "parameter:is_paper == is_paper",
  name = "03_shiny_consolidation",
  parameters = list(is_paper = FALSE)
)

results_agegroup <- readr::read_csv(paste0(
  orderly_root,
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/results_agegroup.csv.gz"
)) %>%
  filter(
    iso3 %in% ssa_iso3,
    !grepl(paste("MMC-nT", "TMIC", sep = "|"), type)
  )
gc()

# results from OOS Validation etc
# dir_name_val <- orderly::orderly_search(
#   name = "val_04_shiny_consolidation",
#   parameters = list(is_paper = FALSE)
# )
dir_name_val <- archives %>% 
  filter(name == "val_04_shiny_consolidation") %>% 
  slice(n()) %>% 
  pull(id)

results_oos_val <- readr::read_csv(file.path(
  orderly_root,
  "/archive/val_04_shiny_consolidation",
  dir_name_val,
  "/artefacts/results_oos.csv.gz"
)) %>%
  filter(iso3 %in% ssa_iso3)

ssa_iso3 <- ssa_iso3[ssa_iso3 %in% results_oos_val$iso3]

results_var_corr <- readr::read_csv(file.path(
  orderly_root,
  "/archive/val_04_shiny_consolidation",
  dir_name_val,
  "/artefacts/results_investigating_var.csv.gz"
)) %>%
  filter(iso3 %in% ssa_iso3)

results_oos_var_corr <- readr::read_csv(file.path(
  orderly_root,
  "/archive/val_04_shiny_consolidation",
  dir_name_val,
  "/artefacts/results_oos_var.csv.gz"
)) %>%
  filter(iso3 %in% ssa_iso3)

# ssa_iso3 <- ssa_iso3[ssa_iso3 %in% results_oos_val$iso3]

# shapefiles
areas_loc <- archives %>%
  filter(name == "00a2_areas_join") %>%
  slice(n()) %>% 
  pull(id)
areas_loc <- file.path(
  orderly_root,
  "archive/00a2_areas_join/",
  areas_loc, 
  "artefacts/areas.geojson"
)
areas <- read_circ_data(areas_loc) %>%
  filter(iso3 %in% ssa_iso3)

# survey data
survey_data <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/survey-circumcision-coverage_shiny.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)


# all_surveys <- readr::read_csv("global/all_surveys.csv")
all_surveys <- readr::read_csv(file.path(
  orderly_root,
  "/archive/val_04_shiny_consolidation",
  dir_name_val,
  "/artefacts/used_surveys.csv.gz"
)) %>%
  filter(iso3 %in% ssa_iso3)

# data which is fed into Shiny app
val_plots_data <- list(
  "ssa_iso3"                    = ssa_iso3,
  "results_agegroup_comparison" = results_agegroup,
  "results_oos_val"             = results_oos_val,
  "results_var_corr"            = results_var_corr,
  "results_oos_var_corr"        = results_oos_var_corr,
  "survey_data"                 = survey_data,
  "all_surveys"                 = all_surveys,
  "orderly_root"                = orderly_root
)

# source function and module, respectively
source(paste0(orderly_root, "/Shiny/src/functions.R"))
source(paste0(
  orderly_root,
  "/Shiny/modules/04_validation_plots/validation_plots_module.R"
))

#### Shiny Instance ####

# UI
ui <- fluidPage(
  fluidRow(
    style = "padding: 20px;",
    column(12, val_plots_UI(id = "comparison_plots1"))
  )
)

# Server
server <- function(input, output, session) {
  callModule(
    module = val_plots_server,
    id = "comparison_plots1",
    selected = reactive(NULL),
    data = val_plots_data
  )
}

shinyApp(ui = ui, server = server)