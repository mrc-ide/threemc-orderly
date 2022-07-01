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

#### Specify orderly_path ####

orderly_root <- here::here()
orderly_root <- unlist(stringr::str_split(orderly_root, "/"))
orderly_root <- orderly_root[1:which(orderly_root == "threemc-orderly")]
orderly_root <- paste(orderly_root, collapse = "/")

#### source modules and functions ####

source(paste0(orderly_root, "/Shiny/src/functions.R"))
source(
  paste0(orderly_root, "/Shiny/modules/01_single_plots/single_plots_module.R")
)
source(
  paste0(orderly_root, "/Shiny/modules/02_ssa_plots/ssa_plots_module.R")
)

#### Data ####

ssa_iso3 <- c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
  "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN",
  "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA",
  "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
)

# shapefile
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
  filter(iso3 %in% ssa_iso3) %>% 
  group_by(area_level) %>%
  dplyr::mutate(space = row_number()) %>%
  ungroup()

# data which is fed into Shiny app
all_plots_data <- list(
  "ssa_iso3" = ssa_iso3,
  "orderly_root" = orderly_root,
  "areas"    = areas
)

#### UI #####

ui = fluidPage(
  # waiter::use_waiter(),
  # waiter::waiter_show_on_load(
  #   html = waiter::spin_loaders(
  #     3, color = "#007bff", style = "font-size:50px;"
  #   ), 
  #   color = waiter::transparent(1.0)
  # ),
  shinyjs::useShinyjs(),
  
  fluidRow(
    style = "padding:20px;",
    column(
      12,
      tabsetPanel(
        tabPanel(
          "Single Country",
          br(),
          br(),
          single_plots_UI("single_plots1"),
        ),
        tabPanel(
          "SSA",
          br(),
          br(),
          ssa_plots_UI("ssa_plots1"),
        )
      )
    )
  )
)


#### Server #####
server <- function(input, output, session) {
  
  #### Call Modules ####
  
  callModule(
    single_plots_server,
    "single_plots1",
    data = all_plots_data
  )
  callModule(
    ssa_plots_server,
    "ssa_plots1",
    data = all_plots_data
  )
  # callModule(supplyServer,'supply1',connection=con,selected=reactive('United Kingdom'),
  #            data=reactive(list("hesitancy_df" = hesitancy_df,"owid" = owid)))
  
}
shinyApp(ui = ui, server = server)