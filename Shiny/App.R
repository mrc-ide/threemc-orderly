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

source(
  paste0(orderly_root, "/Shiny/modules/03_comparison_plots/comparison_plots_module.R")
)

#### Data ####

ssa_iso3 <- comparison_iso3 <- sort(c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
  "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN",
  "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA",
  "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
))

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
  filter(iso3 %in% ssa_iso3) 

areas_join <- areas %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    iso3,       area_id,          area_name,
    area_level, area_level_label, area_sort_order
  ) %>%
  distinct()

areas <- areas %>% 
  group_by(area_level) %>%
  dplyr::mutate(space = row_number()) %>%
  ungroup()

# DMPPT2 data
dmppt2_data <- readr::read_csv(
  paste0(orderly_root, "/Shiny/global/dmppt2-2021_circumcision_coverage.csv.gz")
)

# only use iso3s with DMPPT2 data (?)
comparison_iso3 <- comparison_iso3[comparison_iso3 %in% dmppt2_data$iso3]

# survey_data
survey_data <- read_circ_data(
  paste0(orderly_root, "/Shiny/global/survey-circumcision-coverage.csv.gz"),
  filters = c("sex" = "male")
) %>% 
  filter(iso3 %in% comparison_iso3)

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
all_plots_data <- list(
  "orderly_root"    = orderly_root,
  "ssa_iso3"        = ssa_iso3,
  "areas"           = areas,
  "areas_join"      = areas_join,
  "comparison_iso3" = comparison_iso3,
  "dmppt2_data"     = dmppt2_data,
  "survey_data"     = survey_data
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
        ),
        tabPanel(
          "Comparisons",
          br(),
          br(),
          comparison_plots_UI("comparison_plots1"),
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
    data = all_plots_data,
    selected = reactive("LSO")
  )
  callModule(
    ssa_plots_server,
    "ssa_plots1",
    data = all_plots_data
  )
  callModule(
    comparison_plots_server,
    "comparison_plots1",
    data = all_plots_data,
    selected = reactive("LSO")
  )
}

#### Run App #### 
shinyApp(ui = ui, server = server)