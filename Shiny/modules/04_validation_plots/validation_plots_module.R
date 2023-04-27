#### UI ####

val_plots_UI <- function(id) {
  
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
                  "OOS Validation"                               = "plt_1",
                  "(Co)Variance Hyperparameter Investgation"     = "plt_2",
                  "OOS (Co)Variance Hyperparameter Investgation" = "plt_3"
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
              # plot type for survey plots
              conditionalPanel(
                condition = "input.plot_type == 'plt_1' || 
                input.plot_type == 'plt_2' ||
                input.plot_type == 'plt_3'",
                ns        = ns,
                selectInput(
                  inputId  = ns("spec_type"),
                  label    = "Select Circumcision Types",
                  choices = NULL
                ) 
              ),
              # single choice for age group
              conditionalPanel(
                condition = "input.plot_type == 'plt_1' || 
                input.plot_type == 'plt_2' ||
                input.plot_type == 'plt_3'",
                ns        = ns,
                selectInput(
                  inputId  = ns("age_group_single"),
                  label    = "Select Age Group",
                  choices  = NULL,
                  selected = NULL
                ) 
              ),
              selectInput(
                inputId = ns("take_log"),
                label   = "Take Log",
                choices = c(
                  "TRUE"  = TRUE,
                  "FALSE" = FALSE
                ), 
                selected = FALSE
              ),
              # two-way slider for year
              conditionalPanel(
                condition = "input.plot_type == 'plt_1' || 
                input.plot_type == 'plt_2' ||
                input.plot_type == 'plt_3'",
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
              # area level
              selectInput(
                inputId  = ns("area_levels"),
                label    = "Select Area Levels",
                choices  = NULL,
                selected = NULL, 
                multiple = TRUE
              ),
              conditionalPanel(
                condition = "input.plot_type == 'plt_1' || 
                input.plot_type == 'plt_2' ||
                input.plot_type == 'plt_3'",
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
            )
          )
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
val_plots_server <- function(input, output, session, selected = reactive(NULL), data) {
  
  #### update picker options (initial) ####
  observe({
    # req(input$plot_type)
    
    # initial countries
    choices = data$ssa_iso3
    # based on plot type
    if (input$plot_type == "plt_1") {
      choices <- unique(data$results_oos_val$iso3)
    } else if (input$plot_type == "plt_2") {
      choices <- unique(data$results_var_corr$iso3)
    } else if (input$plot_type == "plt_3") {
      choices <- unique(data$results_oos_var_corr$iso3)
    }
    
    if (!is.null(input$country)) {
      selected <- input$country
    } else selected <- NULL
    
    updateSelectInput(
      session,
      "country",
      choices = choices, 
      selected = selected
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
  
  # circumcision estimates
  circ_data <- reactive({
    req(input$country)
    req(input$plot_type)
    
    # to look at effect of hyperpar change on oos, compare w/ original oos
    if (input$plot_type == "plt_3") {
      x <- data$results_oos_val
    } else {
      x <- data$results_agegroup
    }
    
    return(filter(x, iso3 == input$country))
  })
  
  # oos_val results
  oos_data <- reactive({
    req(input$country)
    req(input$plot_type)
    
    if (input$plot_type == "plt_1") {
      x <- data$results_oos_val
    } else if (input$plot_type == "plt_2") {
      x <- data$results_var_corr
    } else if (input$plot_type == "plt_3") {
      x <- data$results_oos_var_corr 
    }
    
    return(filter(x, iso3 == input$country))
  })
  
  # survey data
  survey_data <- reactive({
    req(input$country)
    req(input$plot_type)
    
    x <- data$survey_data %>% 
      filter(iso3 == input$country) %>% 
      rename(type = indicator) %>%
      mutate(
        type = case_when(
          type == "circumcised"  ~ "MC coverage",
          type == "circ_medical" ~ "MMC coverage",
          TRUE                   ~ "TMC coverage"
        )
      )
    return(x)
  })
 
  #### update plot options (from data) ####
  
  # update spec_type selector based on whether country has only MC coverage or not
  observe({
    req(oos_data())
    
    choices <- unique(circ_data()$type)
    
    updateSelectInput(
      session,
      "spec_type",
      choices = choices, 
      selected = "MC coverage"
    )
  })
  
  # update n_plots selector
  observe({
    req(input$plot_type)
    req(circ_data())
    
    default <- switch(
      input$plot_type,
      "plt_1" = 12,
      12
    )
    
    updateSelectInput(
      session, 
      "n_plot",
      selected = default
    )
  })
  
  # update single age group selector
  observe({
    req(circ_data())
    req(input$plot_type)
    
    default <- "15-49"
    
    updateSelectInput(
      session,
      "age_group_single",
      choices = c(unique(survey_data()$age_group), "10-29"),
      selected = default
    )
  })
  
  # update years slider 
  observe({
    req(circ_data())
    
    # ensure defaults are lowest & highest years needed for each plot
    defaults <- c(
      min(c(
        min(circ_data()$year, na.rm = TRUE),
        min(survey_data()$year, na.rm = TRUE)
      )), 
      max(c(
        max(circ_data()$year, na.rm = TRUE),
        max(survey_data()$year, na.rm = TRUE)
      ))
    )
    
    updateSliderInput(
      session,
      "year_slider",
      min = defaults[1],
      max = defaults[2],
      value = defaults,
      step = 1
    )
  })
  
  # update area levels selector
  observe({
    req(circ_data())
    req(input$plot_type)
    req(input$dataset)
    
    choices <- c(0, 1, 2, 3)
    selected <- c(0, 1)
    
    updateSelectInput(
      session,
      "area_levels",
      choices = choices,
      selected = selected
    )
  })
  
  #### Plot ####
  # plot Circumcision Coverage vs Year 
  plt_data <- reactive({
    
    req(circ_data())
    req(oos_data())
    req(survey_data())
    req(input$plot_type)
    req(input$age_group_single)
    req(input$year_slider)
    req(input$spec_type)
    
    if (input$plot_type == "plt_1") {
      indicator_labels <- c("OOS", "Original")
    } else if (input$plot_type == "plt_2") {
      indicator_labels <- c("With Fixed Hyperpars", "Original")
    } else if (input$plot_type == "plt_3") {
      indicator_labels <- c("OOS W/ Fixed Hyperpars", "OOS")
    }
    
    years <- paste(c(
      min(circ_data()$year), max(circ_data()$year)
    ), collapse = ":")
    
    main_title <- paste0(
      # "15-49 Prevalence ",
      input$age_group_single,
      " Coverage ",
      "(", years, ")",
      " - Black line denotes DMPPT2 coverage,",
      " Blue dots denote survey coverage - "
    )
      
    threemc_val_plt(
      df_results_oos = oos_data(), 
      df_results_orig = circ_data(),
      df_results_survey = survey_data(), 
      all_surveys = filter(data$all_surveys, iso3 == input$country),
      spec_agegroup = input$age_group_single,
      # spec_agegroup = "15-49",
      spec_years = as.numeric(input$year_slider[[1]]):as.numeric(input$year_slider[[2]]), 
      # spec_years = 2010:2021,
      spec_type = input$spec_type, 
      take_log = input$take_log,
      # spec_type = "MC coverage",
      indicator_labels = indicator_labels,
      xlab = "Year", 
      ylab = "Circumcision Coverage", 
      title = main_title, 
      n_plots = as.numeric(input$n_plot)
    )
    
    
    # plt_dmppt2_compare_year(
    #  circ_data(),
    #  add_data()$dmppt2_data,
    #  # add_data()$survey_data,
    #  survey_data,
    #  # age_per = "15-49",
    #  age_per = input$age_group_single,
    #  # years = plt_start_year : 2021,
    #  years = as.numeric(input$year_slider[[1]]):as.numeric(input$year_slider[[2]]), 
    #  area_levels = as.numeric(input$area_levels),
    #  xlab = "Year",
    #  ylab = "Circumcision Coverage",
    #  title = main_title,
    #  n_plots = as.numeric(input$n_plot)
    # )
      
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
      temp <- plt_data()[[as.numeric(input$plot_n)]]
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