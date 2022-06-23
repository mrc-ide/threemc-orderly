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
    titlePanel("Sub-Saharan Africa Circumcision Coverage Plots"),
    fluidRow(
      width = 12,
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            # # Main Options
            tabPanel(
              # strong("Specific Country Plots"),
              strong("?"),
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
    ) # ,
      # download buttons
      # fluidRow(
      #   column(3, downloadButton(ns("country_download"), "Download Country Surplus Data")),
      #   column(3, downloadButton(ns("vaccine_download"), "Download Nested Vaccine-Specific Data"))
      # ),
  )
}

# the 'selected' parameter is used if we want to pass in a default country to 'select' in the initial drop down
# this is useful if we want to link the default selection to a global input, so that the user doesn't have to change multiple inputs all over the app to view statistics for a given country
single_plots_server <- function(input, output, session, connection, selected = reactive(NULL), data) {

  # update picker options
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
  
  # pull in data (later change this to reactively pull in dir_path)
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

  # Plot data for plot of Circumcision Coverage vs Year 
  cov_vs_year_plt_data <- reactive({
    req(results_agegroup())
    
    plt_mc_coverage_prevalence(
      results_agegroup(),
      data$areas,
      spec_age_group = "10+",
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
  
  # Update plot selecter
  observe({
    req(cov_vs_year_plt_data())

    updateSelectInput(
      session,
      "plot",
      choices = seq_len(length(cov_vs_year_plt_data())),
      selected = 1
    )
  })

  output$cov_vs_year_plt <- renderPlot({
    req(cov_vs_year_plt_data())
    req(input$plot)
    
    cov_vs_year_plt_data()[[input$plot]]
  }) 
}