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
    # titlePanel("Circumcision Coverage vs Year for Single Age Group"),
    fluidRow(
      width = 12,
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            # # Main Options
            tabPanel(
              strong("Options"),
              chooseSliderSkin("Modern", color = "#b2b2b2"),
              selectInput(
                inputId = ns("country"),
                label = "Select Country",
                choices = NULL,
                selected = NULL
              ),
              selectInput(
                inputId = ns("plot"),
                label = "Select Plot",
                choices = NULL,
                selected = NULL
              ),
              selectInput(
                inputId = ns("age_group"),
                label = "Select Age Group",
                choices = NULL,
                selected = NULL
              )
            ),
            tabPanel(
              strong("Save Options"),
              chooseSliderSkin("Modern", color = "#b2b2b2"),
              selectInput(
                inputId = ns("units"),
                label = "Units", 
                choices = c("in", "cm", "mm", "px"),
                selected = "in"
              ),
              selectInput(
                inputId = ns("width"),
                label = "Plot Width",
                choices = seq(0, 2000),
                selected = 15
              ),
              selectInput(
                inputId = ns("height"),
                label = "Plot Height",
                choices = seq(0, 1000),
                selected = 11
              )
            ),
          ),
        ),
        # plot
        mainPanel(
          # reactableOutput(ns("mytable"), height = "700px", width = "100%") %>% withSpinner(color = "#0dc5c1")
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
  
  results_agegroup <- reactive({
    req(input$country)
    
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
    output <- results_reader(type = "age groups", dir_path = aggr_loc)
    
    # order by area hierarchy
    output <- order_area_name(output, areas = data$areas)
    
    return(output)
  })
  
  #### update plot options (from data) ####
  
  # update age group selector
  observe({
    req(results_agegroup())
    
    updateSelectInput(
      session,
      "age_group",
      choices = unique(results_agegroup()$age_group),
      selected = "10+"
    )
  })

  #### Plot ####
  # plot Circumcision Coverage vs Year 
  cov_vs_year_plt_data <- reactive({
    req(results_agegroup())
    req(input$age_group)
    
    plt_mc_coverage_prevalence(
      results_agegroup(),
      data$areas,
      spec_age_group = input$age_group,
      spec_years = 2009:2021,
      area_levels = unique(results_agegroup()$area_level), # use all area levels
      spec_model = "No program data",
      main = "Circumcision Coverage vs Year, ",
      # str_save = save_loc_1,
      # save_width = 16,
      # save_height = 7.5,
      n_plots = 1
    )
  })
  
  # Update plot selector
  observe({
    req(cov_vs_year_plt_data())

    updateSelectInput(
      session,
      "plot",
      choices = seq_len(length(cov_vs_year_plt_data())),
      selected = 1
    )
  })

  #### Output Plot ####
  output$cov_vs_year_plt <- renderPlot({
    req(cov_vs_year_plt_data())
    req(input$plot)
    
    print(input$units)
    print(input$width)
    print(input$height)
     
    cov_vs_year_plt_data()[[input$plot]]
  }) 
  
  #### Downloads ####
  # Single plot
  output$single_plot_download <- downloadHandler(
    filename = function() {
      paste0("01_", tolower(input$country), "_coverage_prevalence.png")
    },
    content = function(file) {
      temp <- cov_vs_year_plt_data()[[input$plot]]
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