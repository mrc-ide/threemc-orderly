#### Static Data ####

# aggregations location
dir_path <- "archive/02_aggregations"

#### UI ####

comparison_plots_UI <- function(id) {
  
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
                  # DMPPT2 Comparison Plots
                  "Coverage vs Year"                  = "plt_1",
                  "DMPPT2 - Barchart"                 = "plt_2",
                  "DMPPT2 Coverage vs Model Coverage" = "plt_3",
                  # Survey Comparison Plots
                  "Surveys - Prevalence vs Age Group" = "plt_4"
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
              # conditionalPanel(
              #   condition = "input.plot_type == 'plt_5'",
              #   ns        = ns,
              #   selectInput(
              #     inputId  = ns("spec_type"),
              #     label    = "Select Circumcision Types",
              #     choices  = c(
              #       "Total Circumcision" = "MC coverage",
              #       "Medical Circumcision" = "MMC coverage",
              #       "Traditional Circumcision" = "TMC coverage"
              #     ),
              #     selected = "MC coverage",
              #     multiple = TRUE
              #   ) 
              # ),
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
              # multiple choice for age group
              conditionalPanel(
                condition = "input.plot_type == 'plt_2' || 
                input.plot_type == 'plt_3'",
                ns        = ns,
                selectInput(
                  inputId  = ns("age_group_multiple"),
                  label    = "Select Age Group",
                  choices  = NULL,
                  selected = NULL, 
                  multiple = TRUE
                ) 
              ),
              
              # slider choice for age
              # conditionalPanel(
              #   condition = "input.plot_type == 'plt_2' || 
              #   input.plot_type == 'plt_5' || 
              #   input.plot_type == 'plt_6'",
              #   ns        = ns,
              #   sliderInput(
              #     inputId = ns("age_slider"),
              #     label   = "Select Ages",
              #     min     = 0, 
              #     max     = 60, 
              #     step    = 5,
              #     value   = c(0, 60)
              #   ) 
              # ),
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
              # multiple selector for year
              conditionalPanel(
                condition = "input.plot_type == 'plt_2' || 
                input.plot_type == 'plt_3'",
                ns        = ns,
                selectInput(
                  inputId  = ns("year_select"), # also updated below
                  label    = "Select Years",
                  choices  = NULL,
                  selected = NULL,
                  multiple = TRUE
                )
              ),
              # area level for non-map plots
              # conditionalPanel(
              #   condition = "input.plot_type == 'plt_1' ||
              #   input.plot_type == 'plt_2'",
              #   ns        = ns,
                selectInput(
                  inputId  = ns("area_levels"),
                  label    = "Select Area Levels",
                  choices  = NULL,
                  selected = NULL, 
                  multiple = TRUE
                )
              # )
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
comparison_plots_server <- function(input, output, session, selected = reactive(NULL), data) {

  #### update picker options (initial) ####
  observe({
    updateSelectInput(
      session,
      "country",
      choices = data$comparison_iso3 # convert to country name?
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
  # observe({
  #   req(input$plot_type)
  #   
  #   default <- switch(
  #     input$plot_type,
  #     "plt_2" = c(0, 60),
  #     "plt_5" = c(0, 60),
  #     "plt_6" = c(0, 30)
  #   )
  #   
  #   updateSliderInput(
  #     session,
  #     "age_slider",
  #     value = default
  #   )
  # })
  
  #### Pull in data ####
  
  # aggregated model data
  circ_data <- reactive({
    req(input$country)
    
    # browser()
    
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
    output <- readr::read_csv(
      paste0(aggr_loc, "Results_AgeGroup_Prevalence.csv.gz")
    )
    
    # order by area
    output <- order_area_name(left_join(output, data$areas_join))
    
    return(output)
  })
  
  add_data <- reactive({
    req(input$country)
    
    # browser()
    
    dmppt2_data <- data$dmppt2_data %>% 
      filter(iso3 == input$country)
    survey_data <- data$survey_data %>% 
      filter(iso3 == input$country)
    
    # order by area
    dmppt2_data <- order_area_name(left_join(dmppt2_data, data$areas_join))
    survey_data <- order_area_name(left_join(survey_data, data$areas_join))
    
    # convert dmpppt2 (and survey) age group to our convention
    dmppt2_data <- change_agegroup_convention(dmppt2_data)
    survey_data <- change_agegroup_convention(survey_data)
    
    output <- list(
      "dmppt2_data" = dmppt2_data,
      "survey_data" = survey_data
    )
    
    return(output)
  })
  
  #### update plot options (from data) ####
  
  # update single age group selector
  observe({
    req(circ_data())
    req(add_data())
    req(input$plot_type)
    
    default <- switch(
      input$plot_type,
      "plt_1" = "15-49"
    )
    
    updateSelectInput(
      session,
      "age_group_single",
      # choices = unique(circ_data()$age_group),
      choices = unique(add_data()$dmppt2_data$age_group),
      selected = default
    )
  })
  
  # update multiple age group selector
  observe({
    req(circ_data())
    req(input$plot_type)
    
    default <- switch(
      input$plot_type,
      "plt_2" = c("0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54", "54-59", "60-64")
    )
    
    # only have age bands as choices
    choices <- unique(circ_data()$age_group)
    choices <- choices[!grepl("+", choices)] 
    
    updateSelectInput(
      session,
      "age_group_multiple",
      choices = choices,
      selected = default
    )
  })
  
  # update years slider 
  observe({
    req(circ_data())
    req(add_data())
    
    updateSliderInput(
      session,
      "year_slider",
      # min = min(circ_data()$year, na.rm = TRUE),
      min = min(add_data()$dmppt2_data$year, na.rm = TRUE),
      # max = max(circ_data()$year, na.rm = TRUE),
      max = max(add_data()$dmppt2_data$year, na.rm = TRUE),
      value = c(2009, 2021), 
      step = 1
    )
  })
  
  # update years selector
  observe({
    req(circ_data())
    req(add_data())
    req(input$plot_type)

    # select <- c("2009", "2015", "2021")
    # select <- select[select %in% circ_data()$year]
    if (input$plot_type %in% c("plt_2", "plt_3")) {
      # last year in DMPPT2 data
      DMPPT2_last_year <- max(add_data()$dmppt2_data$year)
      # also include 2013, or whatever is later
      year_2013 <- max(2013, min(add_data()$dmppt2_data$year))
      plt_years <- sort(unique(c(DMPPT2_last_year, year_2013)))
    } else {
      plt_years <- c(min(circ_data()$year), max(circ_data()$year))
    }

    updateSelectInput(
      session,
      "year_select",
      choices = unique(circ_data()$year),
      selected = plt_years
    )
  })
  
  # update area levels selector
  observe({
    req(circ_data())
    req(add_data())
    
    area_levs <- unique(add_data()$dmppt2_data$area_level)
    
    updateSelectInput(
      session,
      "area_levels",
      choices = area_levs,
      selected = area_levs
    )
  })
  
  #### Plot ####
  # plot Circumcision Coverage vs Year 
  plt_data <- reactive({
    
    req(circ_data())
    req(add_data())
    req(input$plot_type)
    
    # ensure survey_data has the right type
    if (input$plot_type %in% c("plt_1", "plt_2", "plt_3")) {
      
      survey_data <- add_data()$survey_data %>% 
        filter(
          (indicator == "circumcised" & iso3 != "LSO") | 
          (indicator == "circ_medical" & iso3 == "LSO")
        )
    } else if (input$plot_type == "plt_4") {
      
      survey_data <- add_data()$survey_data %>% 
        rename(type = indicator) %>% 
        mutate(
          type = case_when(
            type == "circumcised"  ~ "Total",
            type == "circ_medical" ~ "Medical",
            TRUE                   ~ "Traditional"
          )
        )
    }
    
    # Prevalence vs year
    if (input$plot_type == "plt_1") {
      
      req(input$age_group_single)
      req(input$year_slider)
      req(input$area_levels)
      
      years <- paste(c(
        min(circ_data()$year), max(circ_data()$year)
      ), collapse = ":")
      # plt_start_year <- min(circ_data()$year)
      
      main_title <- paste0(
        # "15-49 Prevalence ",
        input$age_group_single,
        " Coverage ",
        "(", years, ")",
        " - Black line denotes DMPPT2 coverage,",
        " Blue dots denote Surveyed coverage - "
      )
      
      # browser()
      
      plt_dmppt2_compare_year(
        circ_data(),
        add_data()$dmppt2_data,
        # add_data()$survey_data,
        survey_data,
        # age_per = "15-49",
        age_per = input$age_group_single,
        # years = plt_start_year : 2021,
        years = as.numeric(input$year_slider[[1]]):as.numeric(input$year_slider[[2]]), 
        area_levels = as.numeric(input$area_levels),
        xlab = "Year",
        ylab = "Circumcision Coverage",
        title = main_title,
        n_plots = 1
      )
      
    } else if (input$plot_type == "plt_2") {
      
      # req(input$area_levels)
      # req(input$age_group_multiple)
      # req(input$year_select)
      
      # last year in DMPPT2 data
      DMPPT2_last_year <- max(add_data()$dmppt2_data$year)
      # also include 2013, or whatever is later
      year_2013 <- max(2013, min(add_data()$dmppt2_data$year))
      plt_years <- sort(unique(c(DMPPT2_last_year, year_2013)))
      
      main_title <- "Circumcision Coverage by Age Group - "
      
      plt_dmppt2_compare_age_group(
        circ_data(),
        add_data()$dmppt2_data,
        # survey_data = survey_data,
        survey_data = NULL, 
        # area_levels = as.numeric(input$area_levels),
        age_per = c(
          "0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", "30-34",
          "35-39", "40-44", "45-49", "50-54", "54-59", "60-64"
        ),
        # age_per = input$age_group_multiple,
        years = plt_years,
        # years = input$year_select, 
        xlab = "Age Group",
        ylab = "Circumcision Coverage",
        title = main_title,
        n_plots = 1
      )
    } else if (input$plot_type == "plt_3") {
      
      req(input$age_group_single)
      req(input$area_levels)
      req(input$year_select)
      
      # main_title <- paste0("10-29 DMPPT2 Prevalence vs threemc Prevalence, ")
      main_title <- paste0(
        input$age_group_single,
        " DMPPT2 Coverage vs threemc Coverage, "
      )
      
      # take for maximum dmppt2 year, and (at least) 2013
      # spec_years <- sort(unique(c(
      #   max(add_data()$dmppt2_data$year), 
      #   max(2013, min(add_data()$dmppt2_data$year))
      # )))
      
      rev(plt_dmppt2_compare_fits(
        circ_data(),
        add_data()$dmppt2_data,
        # age_per = "10-29",
        age_per = input$age_group_single,
        # years =  spec_years, 
        years = input$year_select,
        area_levels = input$area_levels,
        xlab = "DMPPT2 Coverage",
        ylab = "threemc Coverage",
        title = main_title
      ))
    } else if (input$plot_type == "plt_4") {
      
      years <- unique(add_data()$survey_data$year)
      
      main_title = "Circumcision Coverage by Age Group (Black dots denote sampled coverage) - "
      
      plt_MC_modelfit(
        # df_results = results_agegroup,
        df_results = circ_data(),
        # df_results_survey = results_survey,
        df_results_survey = survey_data,
        # mc_type_model = names(types)[x],
        mc_type_model = "MMC coverage",
        # mc_type_survey = types[[x]][[1]],
        mc_type_survey = "Medical",
        age_per = c(
          "0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", "30-34",
           "35-39", "40-44", "45-49", "50-54", "54-59", "60-64"
        ),
        # survey_years =  years[years %in% results_agegroup$year],
        survey_years =  years[years %in% circ_data()$year],
        model_type = "No program data",
        xlab = "Age Group",
        ylab = "Circumcision Coverage",
        # title = paste(types[[x]][[1]], main_title),
        title = paste("Medical", main_title),
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