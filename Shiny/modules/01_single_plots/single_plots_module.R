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
    fluidPage(
      # width = 12,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tabsetPanel(
            #### Options common between plots ####
            tabPanel(
              strong("Common"),
              selectInput(
                inputId = ns("country"),
                label   = "Select Country",
                choices = NULL,
                selected = NULL
              ),
              selectInput(
                inputId = ns("plot_type"),
                label   = "Plot Type", 
                choices = c(
                 "Coverage & Prevalence vs Year"                  = "plt_1",
                 "Probability & Coverage vs Age by Type"          = "plt_2",
                 "Coverage Map"                                   = "plt_3",
                 "Coverage vs Year by Type"                       = "plt_4",
                 "Coverage vs Age"                                = "plt_5",
                 "Ridge Plot of mean TMIC and MMC-nT age"         = "plt_6"
                )
              ),
              selectInput(
                inputId  = ns("plot_n"),
                label    = "Plot Number",
                choices  = NULL,
                selected = NULL
              )
            ),
            #### Options specific to each plot ####
            tabPanel(
              strong("Plot Specific"),
              # type selector for plot 5
              conditionalPanel(
                condition = "input.plot_type == 'plt_5'",
                ns        = ns,
                selectInput(
                  inputId  = ns("spec_type"),
                  label    = "Select Circumcision Types",
                  choices  = c(
                    "Total Circumcision" = "MC coverage",
                    "Medical Circumcision" = "MMC coverage",
                    "Traditional Circumcision" = "TMC coverage"
                  ),
                  selected = "MC coverage",
                  multiple = TRUE
                ) 
              ),
              # single choice for age group
              conditionalPanel(
                condition = "input.plot_type == 'plt_1' || 
                input.plot_type == 'plt_3' || 
                input.plot_type == 'plt_4'",
                ns        = ns,
                selectInput(
                  inputId  = ns("age_group_single"),
                  label    = "Select Age Group",
                  choices  = NULL,
                  selected = NULL
                ) 
              ),
              # slider choice for age
              conditionalPanel(
                condition = "input.plot_type == 'plt_2' || 
                input.plot_type == 'plt_5' || 
                input.plot_type == 'plt_6'",
                ns        = ns,
                sliderInput(
                  inputId = ns("age_slider"),
                  label   = "Select Ages",
                  min     = 0, 
                  max     = 60, 
                  step    = 5,
                  value   = c(0, 60)
                ) 
              ),
              # two-way slider for year
              conditionalPanel(
                condition = "input.plot_type == 'plt_1' || 
                input.plot_type == 'plt_3' ||
                input.plot_type == 'plt_4'",
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
              # multiple selector for year
              conditionalPanel(
                condition = "input.plot_type == 'plt_2' ||
                input.plot_type == 'plt_5' ||
                input.plot_type == 'plt_6'",
                ns        = ns,
                selectInput(
                  inputId  = ns("year_select"), # also updated below
                  label    = "Select Years",
                  choices  = NULL,
                  selected = NULL, 
                  multiple = TRUE
                )
              ),
              conditionalPanel(
                condition = "input.plot_type == 'plt_3'",
                ns        = ns,
                checkboxInput(
                  inputId  = ns("inc_difference"),
                  label    = strong("Include Difference"), 
                  value = TRUE, 
                  width = 4
                )
              ),
              # area level for non-map plots
              conditionalPanel(
                condition = "input.plot_type != 'plt_3'",
                ns        = ns,
                selectInput(
                  inputId  = ns("area_levels"),
                  label    = "Select Area Levels",
                  choices  = NULL,
                  selected = NULL, 
                  multiple = TRUE
                )
              ),
              # separate area level selectors for map plots
              conditionalPanel(
                condition = "input.plot_type == 'plt_3'",
                ns        = ns,
                selectInput(
                  inputId  = ns("border_area_level"),
                  label    = "Select Border Area Level",
                  choices  = NULL,
                  selected = NULL
                )
              ),
              conditionalPanel(
                condition = "input.plot_type == 'plt_3'",
                ns        = ns,
                selectInput(
                  inputId  = ns("results_area_level"),
                  label    = "Select Results Area Level",
                  choices  = NULL,
                  selected = NULL
                )
              ),
              conditionalPanel(
                condition = "input.plot_type == 'plt_4' ||
                input.plot_type == 'plt_5' || 
                input.plot_type == 'plt_6",
                ns        = ns,
                selectInput(
                  inputId = ns("n_plot"),
                  label = "Number of Areas to Display",
                  choices = 1:15,
                  selected = 1
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
single_plots_server <- function(input, output, session, connection, selected = reactive(NULL), data) {

  #### update picker options (initial) ####
  observe({
    updateSelectInput(
      session,
      "country",
      choices = data$ssa_iso3 # convert to country name?
    )

    # update select input based on plot selection
    if (!is.null(selected())) {
      updateSelectInput(
        session,
        "country",
        selected = selected()
      )
    }
  })
  
  # update age range slider (doesn't require results)
  observe({
    req(input$plot_type)
    
    default <- switch(
      input$plot_type,
      "plt_2" = c(0, 60),
      "plt_5" = c(0, 60),
      "plt_6" = c(0, 30)
    )
    
    updateSliderInput(
      session,
      "age_slider",
      value = default
    )
  })
  
  #### Pull in data ####
  
  # results <- reactive({
  #   req(input$country)
  #   req(input$plot_type)
  #   
  #   # find report name for specific country
  #   report_name <- orderly::orderly_search(
  #     name = "02_aggregations",
  #     query = "latest(parameter:cntry == cntry)",
  #     parameters = list(cntry = as.character(input$country)), 
  #     root = data$orderly_root
  #   )
  #   
  #   # location of aggregations to load
  #   aggr_loc <- file.path(
  #     data$orderly_root, dir_path, report_name, "artefacts/"
  #   )
  #   
  #   # load data 
  #   if (input$plot_type %in% c("plt_2", "plt_5", "plt_6")) {
  #     output <- results_reader(type = "age", dir_path = aggr_loc)
  #   } else {
  #     output <- results_reader(type = "age groups", dir_path = aggr_loc)
  #   }
  #   
  #   # order by area hierarchy
  #   output <- order_area_name(output, areas = data$areas)
  #   
  #   return(output)
  # })
  
  results <- reactive({
    req(input$country)
    req(input$plot_type)
    
    # load data
    if (input$plot_type %in% c("plt_2", "plt_5", "plt_6")) {
      output <- filter(data$results_age, iso3 == input$country)
    } else {
      output <- filter(data$results_agegroup, iso3 == input$country)
    }
    
    return(output)
  }) %>% 
    bindCache(input$country, input$plot_type)
  
  # # reactive value for plot height
  # plotheight <- reactive({
  #   req(input$plot_height())
  #   
  #   return(as.numeric(input$plot_height()))
  # })
  
  
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
    
    default <- "10-29"
    
    updateSelectInput(
      session,
      "age_group_single",
      choices = unique(results()$age_group),
      selected = default
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
  
  # update years selector
  observe({
    req(results())
    
    select <- c("2009", "2015", "2021")
    select <- select[select %in% results()$year]
    
    updateSelectInput(
      session,
      "year_select",
      choices = unique(results()$year),
      selected = select
    )
  })
  
  # update area levels selector
  observe({
    req(results())
    
    area_levs <- unique(results()$area_level)
    
    updateSelectInput(
      session,
      "area_levels",
      choices = area_levs,
      selected = area_levs
    )
  })
  
  # update "results" area levels selector
  observe({
    req(results())
    
    area_levs <- unique(results()$area_level)
    
    updateSelectInput(
      session,
      "results_area_level",
      choices = area_levs,
      selected = max(area_levs, na.rm = TRUE)
    )
  })
  
  # update "shapefiles" area levels selector
  observe({
    req(input$country)
    
    area_levs <- data$areas %>% 
      filter(iso3 == input$country) %>% 
      distinct(area_level) %>% 
      pull()
    
    updateSelectInput(
      session,
      "border_area_level",
      choices = area_levs,
      # selected = min(area_levs)
      selected = 1
    )
  })
  
  # update n_plots selector
  observe({
    req(input$plot_type)
    
    # default <- switch(
    #   input$plot_type,
    #   "plt_4" = 12,
    #   "plt_5" = 12,
    #   "plt_6" = 8,
    #   1
    # )
    default <- 12
    
    updateSelectInput(
      session, 
      "n_plot",
      selected = default
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
      req(input$area_levels)
      
      plt_mc_coverage_prevalence(
        results(),
        data$areas,
        spec_age_group = input$age_group_single,
        spec_years = as.numeric(input$year_slider[[1]]):as.numeric(input$year_slider[[2]]),
        # area_levels = unique(results()$area_level), # use all area levels
        area_levels = as.numeric(input$area_levels),
        spec_model = "No program data",
        main = "Circumcision Coverage vs Year, ",
        n_plots = 1
        # n_plots = as.numeric(input$n_plot)
      ) 
      
      # Prevalence vs age for each circumcision type
    } else if (input$plot_type == "plt_2") {
      
      req(input$year_select)
      req(input$area_levels)
      req(input$age_slider)
      
      plt_age_coverage_by_type(
        results(),
        data$areas,
        # spec_years = c(2009, 2015, 2021),
        spec_years = as.numeric(input$year_select),
        # area_levels = unique(results_age$area_level), # use all area levels
        area_levels = as.numeric(input$area_levels), 
        spec_ages = as.numeric(input$age_slider),
        spec_model = "No program data", 
        main = "Circumcision Coverage vs Age, ",
        n_plots = 1
      )
    # Map plot for prevalence   
    } else if (input$plot_type == "plt_3") {
      
      # browser()
      
      req(input$age_group_single)
      req(input$year_slider)
      req(input$results_area_level)
      req(input$border_area_level)
      
      plt_coverage_map(
        results_agegroup   = results(),  
        areas              = data$areas, 
        colourPalette      = colourPalette, 
        spec_countries     = input$country,
        # spec_age_group   = "15-49", 
        spec_age_group     =  input$age_group_single,
        # spec_years = c("2010", "2021"),
        # spec_years         = as.numeric(c(input$year_select[1], input$year_select[2])), 
        spec_years         = as.numeric(input$year_slider),
        results_area_level = input$results_area_level, 
        country_area_level = input$border_area_level,
        inc_difference     = input$inc_difference,
        spec_model         = "No program data", 
        plot_type          = "single",
        n_plots            = 1
      )
      
    
    } else if (input$plot_type == "plt_4") {
      
      req(input$year_slider)
      req(input$age_group_single)
      req(input$area_levels)
      req(input$n_plot)
      
      plt_area_facet_coverage(
        results(),
        data$areas, 
        # spec_years = 2008:2020,
        spec_years = input$year_slider[1]:input$year_slider[2],
        # spec_age_group = "10-29",
        spec_age_group = input$age_group_single,
        # area_levels = unique(results()$area_level),
        area_levels = input$area_levels,
        spec_model = "No program data",
        spec_title = paste0("Male Circumcision Coverage, ",
                            input$year_slider[1], "-", input$year_slider[2],
                            " age ",  input$age_group_single, " years"),
        n_plots = as.numeric(input$n_plot)
      )
      
    } else if (input$plot_type == "plt_5") {
      
      req(input$spec_type)
      req(input$year_select)
      req(input$area_levels)
      req(input$age_slider)
      req(input$n_plot)
      
      plt_age_coverage_multi_years(
        results(),
        data$areas, 
        spec_types = input$spec_type,
        spec_years  = sort(as.numeric(input$year_select)), 
        area_levels = input$area_levels,
        spec_model = "No program data",
        spec_ages = as.numeric(input$age_slider), 
        n_plots = as.numeric(input$n_plot)
      )
      
    } else if(input$plot_type == "plt_6") {
      
      req(input$year_select)
      req(input$area_levels)
      req(input$age_slider)
      req(input$n_plot)
      
      plt_circ_age_ridge(
        results_age = results(), 
        areas = data$areas, 
        spec_years = sort(as.numeric(input$year_select)), 
        area_levels = input$area_levels, 
        spec_model = "No program data", 
        spec_ages = as.numeric(input$age_slider),
        n_plots = as.numeric(input$n_plot)
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