## Static Data ------------------------------------------------------------

# Colour Palette for plot
colourPalette <- rev(colorRampPalette(
  c(
    "#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2"
  )
)(100))


## UI ---------------------------------------------------------------------
surplus_doses_UI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Something Something Something"),
    fluidRow(
      width = 12,
      sidebarLayout(
        position = "right",
        sidebarPanel(
          tabsetPanel(
            # # Main Options
            # tabPanel(
            #   strong("Main"),
            #   chooseSliderSkin("Modern", color = "#b2b2b2"),
            #   pickerInput(ns("country_input"), "Countries", multiple = T, options = list(`actions-box` = TRUE), choices = NULL, selected = NULL),
            #   sliderTextInput(ns("age_range_input"), label = "Age Range (years old)", choices = ages, selected = c("0", "100+")),
            #   sliderInput(ns("threshold_input"), "Population Vaccination Threshold", min = 50, max = 150, post = " %", value = 100, step = 10),
            #   sliderTextInput(ns("end_date_2022"), "2022 End Date", date.end.month_2022, "2022-12-31"),
            #   checkboxInput(ns("approved_vaccines"), strong("Approved Vaccines Only"), FALSE),
            #   checkboxInput(ns("subtract_donations"), strong("Ignore Donations to Date"), FALSE),
            # ),
            # # boosters
            # tabPanel(
            #   strong("Boosters"),
            #   selectizeInput(ns("booster_input"),
            #     label = "Boosters", choices = c("None", "All", "High-risk", "Above Specific Age"),
            #     selected = "None", multiple = F
            #   ),
            #   sliderTextInput(ns("booster_date"), "Booster Start Date", dates, "2021-09-30"),
            #   # conditionalPanel(
            #   #     condition = "input.booster_input == 'Above Specific Age'",
            #   sliderInput(ns("booster_age"), "Minimum Booster Age (for \"Above Specific Age\")", min = 12, max = 99, step = 1, value = 18)
            #   # )
            # ),
            # # Donations and Expirations
            # tabPanel(
            #   strong("Donations & Expirations"),
            #   h4("Donations"),
            #   checkboxInput(ns("donations"), strong("Add Donations"), FALSE),
            #   sliderTextInput(ns("donation_date"), "Donations Start Date", dates, "2021-12-31"),
            #   sliderInput(ns("donation_percentage"), "Donation Percentage (%)", min = 0, max = 50, post = " %", value = 0, step = 2.5),
            #   sliderInput(ns("donation_rampup"), "Monthly Donation Rampup (%)", min = 0, max = 50, post = " %", value = 0, step = 2.5),
            #   sliderInput(ns("donation_cap"), "Donation Cap (%)", min = 0, max = 50, post = " %", value = 0, step = 2.5),
            #   h4("Expirations"),
            #   checkboxInput(ns("expiration"), strong("Allow Vaccine Expiration"), FALSE),
            #   p("Unknown vaccine shelf lives assumed to be 6 months")
            # )
            # Plot of 
            tabPanel(
              strong("Coverage vs Year"),
              shiny::selectizeInput()
          )
        ),
        # reactable table itself
        mainPanel(
          reactableOutput(ns("mytable"), height = "700px", width = "100%") %>% withSpinner(color = "#0dc5c1")
        )
      )
    ),
    # download buttons
    fluidRow(
      column(3, downloadButton(ns("country_download"), "Download Country Surplus Data")),
      column(3, downloadButton(ns("vaccine_download"), "Download Nested Vaccine-Specific Data"))
    ),
    # assumptions
    fluidRow(
      br(),
      br(),
      p("These projections are based on supply forecasts for each country. Vaccines still in development have assumed start dates for their supply based on the current stage of development and clinical trial forecasts. \n"),
      p("Surplus doses are calculated under the assumption that countries have the same hesitancy rates (depending on scenario), and the same eligible/susceptible populations. It is also assumed that doses are not stockpiled beyond the set demand scenario, and the vaccines are not reconfigured to second generation vaccines."),
      p("Booster shots assume one dose per eligible person, per year.")
    )
   )
  )
}

## Server -----------------------------------------------------------------
surplus_doses_server <- function(input, output, session, data) {

  ## Data + Slider Config -----------------------------------------------
  # List of Countries to choose from
  europe <- c("Norway", "Iceland", "Liechtenstein", "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
  observe({
    country_list <- data$forecast_country_supply %>%
      distinct(country) %>%
      pull() %>%
      sort()
    # country_list <- c(country_list, sort(europe))
    updatePickerInput(session, "country_input", choices = country_list)
  })
  # create observed donations data if required and not provided
  # donations_data <- reactive({
  #   selection <- input$country_input
  #   if (input$subtract_donations == T) {
  #     if (is.null(donations_df)) {
  #       output <- suppressWarnings(country_supply_observed(type = "donation", remove_eu_countries = F))
  #     } else output <- donations_df
  #   } else output <- donations_df
  #   return(output)
  # })

  # update donation cap with donation threshold
  observe({
    if (input$donation_cap < input$donation_percentage) {
      updateSliderInput(session, "donation_cap", min = 0, max = 50, value = input$donation_percentage, step = 2.5)
    }
  })
  # don't allow age ranges to go above 25 or below 65
  observe({
    if (as.numeric(input$age_range_input[1]) > 25) {
      updateSliderTextInput(session, "age_range_input", choices = ages, selected = c("25", input$age_range_input[2]))
    }
    if (as.numeric(regmatches(input$age_range_input[[2]], gregexpr("[[:digit:]]+", input$age_range_input[[2]]))) < 65) {
      updateSliderTextInput(session, "age_range_input", choices = ages, selected = c(input$age_range_input[1], "65"))
    }
  })
  # don't allow booster age range to exceed (age_range[[2]] - 5)
  observe({
    booster_age_max <- as.numeric(regmatches(
      input$age_range_input[[2]],
      gregexpr(
        "[[:digit:]]+",
        input$age_range_input[[2]]
      )
    )) - 5
    if (as.numeric(input$booster_age) >= booster_age_max) {
      updateSliderInput(session, "booster_age", min = 12, max = 99, step = 1, value = booster_age_max)
    }
  })
  # filter forecast data for selected countries, keep EU if EU countries
  # are specified, as it is required
  # forecast_data <- reactive({
  #   selection <- input$country_input
  #   if (!is.null(selection)) {
  #     if (sum(europe %in% selection) != 0) {
  #       selection <- c(selection, "European Union")
  #     }
  #     output <- data$forecast_country_supply %>%
  #       filter(country %in% selection)
  #   } else output <- forecast_country_supply
  #   return(output)
  # })

  # filter forecast data for selected countries
  forecast_data <- reactive({
    selection <- input$country_input
    if (!is.null(selection)) {
      output <- data$forecast_country_supply %>%
        filter(country %in% selection)
    } else {
      output <- data$forecast_country_supply %>%
        filter(!country %in% europe)
    }
    return(output)
  })

  # 2022 end date, to be used as column title in table
  end_month <- reactive({
    # (paste("Surplus Doses", format(input$end_date_2022, "%B %Y"), sep = " "))
    (paste("surplus_doses", format(as_date(input$end_date_2022), "%m_%Y"), sep = "_"))
  })

  ## Surplus Data -------------------------------------------------------
  surplus_data <- reactive({

    # pop threshold and age range
    threshold <- as.numeric(input$threshold_input) / 100
    age_range <- paste(input$age_range_input[1], input$age_range_input[2], sep = "-")
    # should boosters be applied?
    if (input$booster_input == "None") booster <- F else booster <- T
    if (input$booster_input == "High-risk") high_risk_booster <- T else high_risk_booster <- F
    if (input$booster_input == "Above Specific Age") age_booster <- T else age_booster <- F

    (surplus_doses(
      # main options (countries, pop + ages, approved vaccs, subtract donations)
      forecast_supply = forecast_data(),
      supply = data$supply_df,
      threshold = threshold,
      end_date_2022 = input$end_date_2022,
      age_df_loc = data$age_demographics,
      age_range = age_range,
      key = data$key,
      approved_vaccines_only = input$approved_vaccines,
      spec_countries = input$country_input,
      subtract_donations = input$subtract_donations,
      donations_df = data$donations_df,
      # booster options
      booster = booster,
      high_risk_booster = high_risk_booster,
      age_booster = age_booster,
      booster_age = input$booster_age,
      booster_start_date = input$booster_date,
      # donation scenario options
      donations = input$donations,
      donation_date = input$donation_date,
      donation_percentage = input$donation_percentage,
      donation_rampup = input$donation_rampup,
      donation_cap = input$donation_cap,
      # expiration scenario options
      expiry = input$expiration,
      expiry_df = data$expiration_dates_df
    ))
  })

  ## Reactable Table ----------------------------------------------------
  output$mytable <- renderReactable({
    req(surplus_data())
    # browser()
    first_table_df <- surplus_data()[[1]] %>%
      mutate(surplus_doses_2021 = round(surplus_doses_2021), surplus_doses_2022 = round(surplus_doses_2022)) %>%
      rename(
        `Country` = country,
        `Threshold Date` = threshold_date,
        `Surplus Doses 2021` = surplus_doses_2021,
        # end_month() := surplus_doses_2022
        `Surplus Doses 2022` := surplus_doses_2022
      )
    nested_table_df <- surplus_data()[[2]] %>%
      rename("Country" = country, "Year" = identifier, "Total" = total_doses)

    tab <- reactable(
      data = first_table_df,
      defaultPageSize = 12, # show 12 countries
      defaultSorted = list("Country" = "desc"), # sort by descending 2021 Surplus Doses by default
      columns = list(
        `Surplus Doses 2021` = colDef(format = colFormat(separators = T, digits = 0)),
        # end_month() = colDef(format = colFormat(separators = T, digits = 0))
        `Surplus Doses 2022` = colDef(format = colFormat(separators = T, digits = 0))
      ),
      searchable = T,
      details = function(index) {
        # find the rows with vaccine specific data for the country in question
        nested_table_df_spec <- nested_table_df %>%
          filter(Country %in% first_table_df$Country[index]) %>%
          select(-Country)
        # remove columns with all 0s
        nested_table_df_spec <- Filter(function(x) !all(x == 0), nested_table_df_spec)
        if (length(nested_table_df_spec) == 0) {
          nested <- NULL
        } else {
          nested <- reactable(nested_table_df_spec)
          htmltools::div(style = list(margin = "12px 45px"), nested)
        }
      },
      onClick = "expand",
      rowStyle = list(cursor = "pointer")
    )
  })

  ## Downloads ----------------------------------------------------------
  # Country Specific Data
  output$country_download <- downloadHandler(
    filename = function() {
      paste("country_surplus_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      temp <- surplus_data()[[1]] %>%
        rename(!!end_month() := surplus_doses_2022)
      fwrite(temp, file)
    }
  )

  # Vaccine Specific Data
  output$vaccine_download <- downloadHandler(
    filename = function() {
      paste("vaccine_surplus_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      temp <- surplus_data()[[2]] %>%
        mutate(
          identifier := ifelse(identifier == "2022 Excess Doses",
            paste(!!end_month(), "Excess Doses", sep = " "),
            identifier
          )
        )
      fwrite(temp, file)
    }
  )
}
