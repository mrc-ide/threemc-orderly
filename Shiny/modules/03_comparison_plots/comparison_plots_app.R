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

# comparison_iso3 <- c(
#   "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", 
#   "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", 
#   "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA", 
#   "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
# )

# temp use a few countries to test
comparison_iso3 <- c("LSO")

# shapefiles
areas_loc <- orderly::orderly_list_archive() %>%
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
  filter(iso3 %in% comparison_iso3)

areas_join <- areas %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    iso3,       area_id,          area_name,
    area_level, area_level_label, area_sort_order
  ) %>%
  distinct()

# DMPPT2 data
dmppt2_data <- readr::read_csv(
  paste0(orderly_root, "/global/dmppt2-2021_circumcision_coverage.csv.gz")
)

# only use iso3s with DMPPT2 data (?)
comparison_iso3 <- comparison_iso3[comparison_iso3 %in% dmppt2_data$iso3]

# survey_data
survey_data <- read_circ_data(
  paste0(orderly_root, "/global/survey-circumcision-coverage.csv.gz"),
  filters = c("sex" = "male")
) %>% 
  filter(
    iso3 %in% comparison_iso3 # ,
    # for LSO DMPPT2 data is for all circumcisions, except for in LSO
    # (indicator == "circumcised" & iso3 != "LSO") | 
    # (indicator == "circ_medical" & iso3 == "LSO")
  )

# change naming convention of survey data (not working currently!)
if ("survey_mid_calendar_quarter" %in% names(survey_data)) {
  survey_data <- survey_data %>%
    rename(
      year = survey_mid_calendar_quarter,
      mean = estimate,
      sd = std_error,
      lower = ci_lower,
      upper = ci_upper
    )
}

# data which is fed into Shiny app
comparison_plots_data <- list(
  "comparison_iso3" = comparison_iso3,
  "dmppt2_data"     = dmppt2_data,
  "survey_data"     = survey_data,
  "orderly_root"    = orderly_root,
  "areas"           = areas,
  "areas_join"      = areas_join
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
    selected = reactive("LSO"),
    data = comparison_plots_data
  )
}

shinyApp(ui = ui, server = server)
