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
    fluidRow(
      width = 12,
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            #### Options common between plots ####
            tabPanel(
              strong("Common"),
              selectInput(
                inputId = ns("country"),
                label = "Select Country",
                choices = NULL,
                selected = NULL
              ),
              selectInput(
                inputId = ns("plot_type"),
                label = "Plot Type", 
                choices = c(
                 "Prevalence vs Year" = "plt_1",
                 "Prevalence vs Age"  = "plt_2"
                )
              ),
              selectInput(
                inputId = ns("plot_n"),
                label = "Plot Number",
                choices = NULL,
                selected = NULL
              ),
              # selectInput(
              #   inputId = ns("n_plot"),
              #   label = "Number of Areas to Display",
              #   choices = 1:10, 
              #   selected = 1
              # ),
              selectInput(
                inputId = ns("area_levels"),
                label = "Select Area Levels",
                choices = NULL,
                selected = NULL, 
                multiple = TRUE
              ),
            ),
            #### Options specific to each plot ####
            tabPanel(
              strong("Plot Specific"),
              # single choice for age group
              conditionalPanel(
                condition = "input.plot_type == 'plt_1'",
                ns = ns,
                selectInput(
                  inputId = ns("age_group_single"),
                  label = "Select Age Group",
                  choices = NULL,
                  selected = NULL
                ) 
              ),
              # slider choice for age
              conditionalPanel(
                condition = "input.plot_type == 'plt_2'",
                ns = ns,
                sliderInput(
                  inputId = ns("age_slider"),
                  label = "Select Ages",
                  min = 0, 
                  max = 60, 
                  step = 5,
                  value = c(0, 60)
                ) 
              ),
              # two-way slider for year
              conditionalPanel(
                condition = "input.plot_type == 'plt_1'",
                ns = ns,
                sliderInput(
                  inputId = ns("year_slider"), # also updated below
                  label = "Select Years",
                  min = 2009,
                  max = 2021,
                  value = c(2009, 2021)
                )
              ),
              # multiple selector for year
              conditionalPanel(
                condition = "input.plot_type == 'plt_2'",
                ns = ns,
                selectInput(
                  inputId = ns("year_select"), # also updated below
                  label = "Select Years",
                  choices = NULL,
                  selected = NULL, 
                  multiple = TRUE
                )
              ),
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
        # Output plot 
        mainPanel(
          plotOutput(outputId = ns("cov_vs_year_plt")) %>% 
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
  
  #### Pull in data ####
  
  results <- reactive({
    req(input$country)
    req(input$plot_type)
    
    # find report name for specific country
    report_name <- orderly::orderly_search(
      name = "02_aggregations",
      query = "latest(parameter:cntry == cntry)",
      parameters = list(cntry = as.character(input$country)), 
      root = data$orderly_root
    )
    
    # location of aggregations to load
    aggr_loc <- file.path(
      data$orderly_root, dir_path, report_name, "artefacts/"
    )
    
    # load data 
    if (input$plot_type %in% c("plt_2")) {
      output <- results_reader(type = "age", dir_path = aggr_loc)
    } else {
      output <- results_reader(type = "age groups", dir_path = aggr_loc)
    }
    
    # order by area hierarchy
    output <- order_area_name(output, areas = data$areas)
    
    return(output)
  })
  
  #### update plot options (from data) ####
  
  # update single age group selector
  observe({
    req(results())
    
    updateSelectInput(
      session,
      "age_group_single",
      choices = unique(results()$age_group),
      selected = "10+"
    )
  })
  
  # update ages slider 
  # observe({
  #   req(results())
  #   
  #   updateSliderInput(
  #     session,
  #     "age_slider",
  #     min = min(results()$age, na.rm = TRUE),
  #     max = max(results()$age, na.rm = TRUE),
  #     value = c(0, 60),
  #     step = 5
  #   )
  # })
  
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
  
  #### Plot ####
  # plot Circumcision Coverage vs Year 
  cov_vs_year_plt_data <- reactive({
    req(results())
    req(input$plot_type)
    if (input$plot_type == "plt_1") {
      req(input$age_group_single)
    } else if (input$plot_type == "plt_2") {
      req(input$age_slider)
    }
    
    # Prevalence vs year
    if (input$plot_type == "plt_1") {
      
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
    }
  })
  
  # Update plot selector
  observe({
    req(cov_vs_year_plt_data())

    updateSelectInput(
      session,
      "plot_n",
      choices = seq_len(length(cov_vs_year_plt_data())),
      selected = 1
    )
  })
  
  #### Output Plot ####
  output$cov_vs_year_plt <- renderPlot({
    req(cov_vs_year_plt_data())
    req(input$plot_n)
    
    cov_vs_year_plt_data()[[input$plot_n]]
  }) 
  
  #### Downloads ####
  # Single plot
  output$single_plot_download <- downloadHandler(
    filename = function() {
      paste0("01_", tolower(input$country), "_coverage_prevalence.png")
    },
    content = function(file) {
      temp <- cov_vs_year_plt_data()[[input$plot_n]]
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
          rlang::squash(cov_vs_year_plt_data()), nrow = 1, ncol = 1
        ), 
        dpi = "retina",
        units = input$units,
        width = as.numeric(input$width),
        height = as.numeric(input$height)
      )
    }
  )
}