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

# results for age groups for comparisons
archives <- orderly::orderly_list_archive()
dir_name <- orderly::orderly_list_archive() %>% 
  filter(name == "03_shiny_consolidation") %>%
  slice(n()) %>% 
  pull(id)
results_agegroup_comparison <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/results_agegroup_comparison.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)

ssa_iso3 <- ssa_iso3[ssa_iso3 %in% results_agegroup_comparison$iso3]

results_agegroup_national_comparison <-  readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/results_agegroup_national_comparison.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)

results_agegroup_probs <-  readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/results_agegroup.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3, grepl("probability", type))

# empirical rates
empirical_rates <-  readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/empirical_rates.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)



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

# DMPPT2 data
# dmppt2_data <- readr::read_csv(
#   paste0(orderly_root, "/Shiny/global/dmppt2-2021_circumcision_coverage.csv.gz")
# )

dmppt2_data <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/dmppt2-2021_circumcision_coverage_shiny.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)

dmppt2_iso3 <- unique(dmppt2_data$iso3)

# survey_data
# survey_data <- read_circ_data(
#   paste0(orderly_root, "/Shiny/global/survey-circumcision-coverage.csv.gz"),
#   filters = c("sex" = "male")
# ) %>% 
#   filter(iso3 %in% ssa_iso3)
survey_data <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/survey-circumcision-coverage_shiny.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)

# data which is fed into Shiny app
comparison_plots_data <- list(
  "ssa_iso3"                    = ssa_iso3,
  "results_agegroup_comparison" = results_agegroup_comparison,
  "results_agegroup_national_comparison" = results_agegroup_national_comparison,
  "dmppt2_iso3"                 = dmppt2_iso3,
  "dmppt2_data"                 = dmppt2_data,
  "survey_data"                 = survey_data,
  "empirical_rates"             = empirical_rates,
  "results_agegroup_probs"      = results_agegroup_probs,
  "orderly_root"                = orderly_root
)

# source function and module, respectively
source(paste0(orderly_root, "/Shiny/src/functions.R"))
source(paste0(
  orderly_root,
  "/Shiny/modules/03_comparison_plots/comparison_plots_module.R"
))

#### Shiny Instance ####

# UI
ui <- fluidPage(
  fluidRow(
    style = "padding: 20px;",
    column(12, comparison_plots_UI(id = "comparison_plots1"))
  )
)

# Server
server <- function(input, output, session) {
  callModule(
    module = comparison_plots_server,
    id = "comparison_plots1",
    selected = reactive(NULL),
    data = comparison_plots_data
  )
}

shinyApp(ui = ui, server = server)
