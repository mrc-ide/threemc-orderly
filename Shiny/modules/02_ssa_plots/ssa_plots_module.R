#### Static Data ####

# aggregations location
dir_path <- "archive/02_aggregations"

# Colour Palette for plot
colourPalette <- rev(colorRampPalette(
  c(
    "#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2"
  )
)(100))

#### UI ####

single_plots_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # fluidPage(
    fillPage(
      # width = 12,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tabsetPanel(
            #### Options common between plots ####
            tabPanel(
              strong("Common"),
              # selectInput(
              #   inputId = ns("country"),
              #   label   = "Select Country",
              #   choices = NULL,
              #   selected = NULL
              # ),
              # selectInput(
              #   inputId = ns("plot_type"),
              #   label   = "Plot Type", 
              #   choices = c(
              #    "Coverage & Prevalence vs Year"                 = "plt_1",
              #    "Prevalence & Coverage vs Age by Type"          = "plt_2",
              #    "Prevalence Map"                                = "plt_3",
              #    "Prevalence vs Year by Type"                    = "plt_4",
              #    "Coverage vs Age"                               = "plt_5",
              #    "Ridge Plot of mean TMIC and MMC-nT age"        = "plt_6"
              #   )
              # ),
              selectInput(
                inputId = ns("plot_type"),
                label   = "Plot Type",
                choices = c(
                 "SSA Circumcision Coverage" = "plt_1"
                )
              ),
              selectInput(
                inputId  = ns("plot_n"),
                label    = "Plot Number",
                choices  = NULL,
                selected = NULL
              ),
              # # leave out for now
              # numericInput(
              #   inputId = ns("plot_height"),
              #   label = "Plot Height (pixels)", 
              #   value = 800, 
              #   min = 400, 
              #   max = 1600, 
              #   step = 50 
              # )
              # selectInput(
              #   inputId = ns("n_plot"),
              #   label = "Number of Areas to Display",
              #   choices = 1:10, 
              #   selected = 1
              # ),
              # move to plot-specific options
              # selectInput(
              #   inputId = ns("area_levels"),
              #   label = "Select Area Levels",
              #   choices = NULL,
              #   selected = NULL, 
              #   multiple = TRUE
              # )
            ),
            #### Options specific to each plot ####
            tabPanel(
              strong("Plot Specific"),
              # single choice for age group
              conditionalPanel(
                condition = "input.plot_type == 'plt_1'",
                ns        = ns,
                selectInput(
                  inputId  = ns("age_group_single"),
                  label    = "Select Age Group",
                  choices  = NULL,
                  selected = NULL
                ) 
              ),
              # two-way slider for year
              conditionalPanel(
                condition = "input.plot_type == 'plt_1'",
                ns        = ns,
                sliderInput(
                  inputId = ns("year_slider"), # also updated below
                  label   = "Select Years",
                  min     = 2009,
                  max     = 2021, 
                  value   = c(2009, 2021),
                  sep     = ""
                )
              ),
              # separate area level selectors for map plots
              conditionalPanel(
                condition = "input.plot_type == 'plt_1'",
                ns        = ns,
                selectInput(
                  inputId  = ns("border_area_level"),
                  label    = "Select Border Area Level",
                  choices  = NULL,
                  selected = NULL
                )
              ),
              conditionalPanel(
                condition = "input.plot_type == 'plt_1'",
                ns        = ns,
                selectInput(
                  inputId  = ns("results_area_level"),
                  label    = "Select Results Area Level",
                  choices  = NULL,
                  selected = NULL
                )
              )
            ),
            #### Options for saving plots ####
            tabPanel(
              strong("Save"),
              chooseSliderSkin("Modern", color = "#b2b2b2"),
              selectInput(
                inputId = ns("units"),
                label = "Units", 
                choices = c("in", "cm", "mm", "px"),
                selected = "in"
              ),
              numericInput(
                inputId = ns("width"),
                label   = "Plot Width",
                value   = 15
              ),
              numericInput(
                inputId = ns("height"),
                label   = "Plot Height",
                value   = 11
              ),
            ),
          ),
        ),
        #### Output plot ####
        mainPanel(
          width = 9, 
          plotOutput(outputId = ns("cov_vs_year_plt"), height = 800) %>% 
            withSpinner(color = "#0dc5c1")
        ),
        position = "right"
      )
    ),
      # download buttons
    fluidRow(
      column(3, downloadButton(ns("single_plot_download"), "Download Single Plot")),
      column(3, downloadButton(ns("all_plots_download"), "Download All Plots"))
    )
  )
}

#### Server ####
single_plots_server <- function(input, output, session, data) {

  #### update picker options (initial) ####
  
  #### Pull in data ####
  
  results <- reactive({
    
    # req(input$plot_type)
    
    # find report names for all countries
    report_names <- sapply(data$ssa_iso3, function(x) {
      orderly::orderly_search( 
        name = "02_aggregations",
        query = "latest(parameter:cntry == cntry)",
        parameters = list(cntry = x), 
        root = data$orderly_root
      )
    })
    # order and remove nas
    report_names <- report_names[order(names(report_names))]
    report_names <- report_names[!is.na(report_names)]
    
    # location of aggregation files to load
    aggr_loc <- file.path(
      data$orderly_root, dir_path, report_names, "artefacts/"
    )
    files <- list.files(aggr_loc, full.names = TRUE, pattern = "AgeGroup_Prevalence")
    
    # load data 
    output <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE))
    
    # order by area hierarchy
    # output <- order_area_name(output, areas = data$areas)
    
    return(output)
  })
  
  #### update plot options (from data) ####
  
  # update single age group selector
  observe({
    req(results())
    req(input$plot_type)
    
    # default <- switch(
    #   input$plot_type,
    #   "plt_1" = "10+",
    #   "plt_3" = "15-49",
    #   "plt_4" = "15-49"
    # )
    
    updateSelectInput(
      session,
      "age_group_single",
      choices = unique(results()$age_group),
      selected = "15-49"
    )
  })
  
  
  # update years slider 
  observe({
    req(results())
    
    updateSliderInput(
      session,
      "year_slider",
      min = min(results()$year, na.rm = TRUE),
      max = max(results()$year, na.rm = TRUE),
      value = c(2009, 2021), 
      step = 1
    )
  })
  
  # update "results" area levels selector
  observe({
    req(results())
    
    # area_levs <- unique(results()$area_level)
    
    updateSelectInput(
      session,
      "results_area_level",
      # choices = area_levs,
      choices = c(0, 1),
      # selected = max(area_levs)
      selected = 1
    )
  })
  
  # update "shapefiles" area levels selector
  observe({
    
    # find "lowest max" area level amongst all countries
    # NOTE: May have to change this a bit
    # lowest_area_lev <- data$areas %>% 
    #   group_by(iso3) %>% 
    #   filter(area_level == max(area_level)) %>% 
    #   ungroup() %>% 
    #   summarise(area_level = min(area_level)) %>% 
    #   pull()
    
    
    updateSelectInput(
      session,
      "border_area_level",
      # choices = 0:lowest_area_lev,
      choices = 0:1, 
      # selected = lowest_area_lev
      selected = 0
    )
  })
  
  
  #### Plot ####
  # plot Circumcision Coverage vs Year 
  plt_data <- reactive({
    
    req(results())
    req(input$plot_type)
    
    # Prevalence vs year
    if (input$plot_type == "plt_1") {
      
      req(input$age_group_single)
      req(input$year_slider)
      req(input$results_area_level)
      req(input$border_area_level)
      
      
      # browser()
      
      plt_coverage_map(
        results(),
        data$areas,
        colourPalette = colourPalette,
        spec_age_group = input$age_group_single,
        spec_years = c(as.numeric(input$year_slider[[1]]), as.numeric(input$year_slider[[2]])),
        spec_model = "No program data",
        plot_type = "map",
        results_area_level = input$results_area_level,
        country_area_level = input$border_area_level, 
        n_plots = 1
      )
    }   
  })
  
  #### Update plot selector ####
  observe({
    req(plt_data())

    updateSelectInput(
      session,
      "plot_n",
      choices = seq_len(length(plt_data())),
      selected = 1
    )
  })
  
  #### Output Plot ####
  output$cov_vs_year_plt <- renderPlot({
    req(plt_data())
    req(input$plot_n)
    
    # if (input$plot_type == "plt_3") browser()
    plt_data()[[as.numeric(input$plot_n)]]
  })
  
  #### Downloads ####
  # Single plot
  output$single_plot_download <- downloadHandler(
    filename = function() {
      paste0("01_", tolower(input$country), "_coverage_prevalence.png")
    },
    content = function(file) {
      temp <- plt_data()[[input$plot_n]]
      ggplot2::ggsave(
        filename = file, 
        plot = temp, 
        dpi = "retina",
        units = input$units,
        width = as.numeric(input$width),
        height = as.numeric(input$height)
      )
    }
  )
  
  # All Plots
  output$all_plots_download <- downloadHandler(
    
    filename = function() {
      paste0("01_", tolower(input$country), "_coverage_prevalence.pdf")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file, 
        plot = gridExtra::marrangeGrob(
          rlang::squash(plt_data()), nrow = 1, ncol = 1
        ), 
        dpi = "retina",
        units = input$units,
        width = as.numeric(input$width),
        height = as.numeric(input$height)
      )
    }
  )
}