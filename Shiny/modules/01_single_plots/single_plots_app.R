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
# orderly_root <- here::here() # run with basename(getwd()) == "threemc-orderly"
orderly_root <- here::here()
orderly_root <- unlist(stringr::str_split(orderly_root, "/"))
orderly_root <- orderly_root[1:which(orderly_root == "threemc-orderly")]
orderly_root <- paste(orderly_root, collapse = "/")

ssa_iso3 <- c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
  "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN",
  "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA",
  "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
)

# temp use a few countries to test
# ssa_iso3 <- c("LSO")

# results for age groups and single ages
archives <- orderly::orderly_list_archive()
dir_name <- orderly::orderly_list_archive() %>% 
  filter(name == "03_shiny_consolidation") %>%
  slice(n()) %>% 
  pull(id)
results_agegroup <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/results_agegroup.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)
results_age <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/results_age.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)

ssa_iso3 <- ssa_iso3[ssa_iso3 %in% results_age$iso3]

# shapefile
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

# data which is fed into Shiny app
single_plots_data <- list(
  "results_age"      = results_age,
  "results_agegroup" = results_agegroup,
  "ssa_iso3"         = ssa_iso3,
  "orderly_root"     = orderly_root,
  "areas"            = areas
)

# source function and module, respectively
source(paste0(orderly_root, "/Shiny/src/functions.R"))
source(paste0(
  orderly_root,
  "/Shiny/modules/01_single_plots/single_plots_module.R"
))

#### Shiny Instance ####

# UI
ui <- fluidPage(
  fluidRow(
    style = "padding: 20px;",
    column(12, single_plots_UI(id = "single_plots_1"))
  )
)

# Server
server <- function(input, output, session) {
  callModule(
    module = single_plots_server,
    id = "single_plots_1",
    selected = reactive("LSO"),
    data = single_plots_data
  )
}

shinyApp(ui = ui, server = server)
