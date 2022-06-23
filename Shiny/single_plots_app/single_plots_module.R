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
    ## Plot Options (might want to convert to side bar rather than row)
    fluidRow(
      # select country
      column(
        width = 3,
        selectInput(
          inputId = ns("country"),
          label = "Select Country",
          choices = NULL,
          selected = NULL # ,
          # width = "100%"
        )
      ),
      # select plot
      column(
        width = 3,
        selectInput(
          inputId = ns("plot"),
          label = "Select Plot",
          choices = NULL,
          selected = NULL,
          width = "100%"
        )
      )
      
      # column(width = 2,
      #        selectInput(
      #          inputId = ns("y_scale"),
      #          label = "Y-Axis Scale",
      #          choices = c("Absolute", "Percentage"),
      #          # selected = "Absolute"
      #          selected = "Percentage")
      #        # prettyRadioButtons(ns("y_scale"),label="Y-Axis Scale",
      #        # c("Absolute", "Percentage"),"Percentage",inline=T)
      # )
    ),
    # Plots
    fluidRow(
      column(
        width = 10,
        tabsetPanel(
          id = "tabs",
          # type = "tabs",
          type = "pills",
          # tabPanel(
          #   "Vaccination Forecast",
          #   id = "vaccination_forecast",
          #   plotlyOutput(ns("plot1")),
          #   br(),
          #   downloadButton(ns("download_plot1"), "Download Data")
          # ),
          # tabPanel(
          #   "First vs Second Dose",
          #   id = "first_vs_second_dose",
          #   plotlyOutput(ns("plot2")),
          #   br(),
          #   downloadButton(ns("download_plot2"), "Download Data")
          # ),
          # tabPanel(
          #   "Supply Forecast",
          #   id = "supply_forecast",
          #   plotlyOutput(ns("plot3")),
          #   tags$em(p("This chart is a projected timeline of vaccine supply, broken down by the specific vaccines received")),
          #   br(),
          #   downloadButton(ns("download_plot3"), "Download Supply Forecast")
          # ),
          # tabPanel(
          #   "Other",
          #   id = "other",
          #   plotlyOutput(ns("plot4")),
          #   br(),
          #   downloadButton(ns("download_plot4"), "Download Data")
          # )
          tabPanel(
            # "Coverage vs Year", 
            id = "cov_vs_year", 
            plotOutput(outputId = ns("cov_vs_year_plt"))
            # br(),
            # downloadButton(ns("download_plot1"), "Download Plot")
          )
        )
      )
    )
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
  }, width = 1400)
  # }) 
  
  ## output plot of Actual vs Expected People Fully Vaccinated (+ Protected Pop) ####
  # output$plot1 <- plotly::renderPlotly({
  #   req(input$country)
  #   req(i())
  # 
  #   if (sum(i()$matrix$vaccinated_75pc_adults) > 0) {
  #     ms1 <- i()$matrix$total[which(i()$matrix$vaccinated_75pc_adults == 1)[[1]]]
  #     h <- i()$vaccinated_75pc_adults
  # 
  #     if (input$y_scale == "Percentage") ms <- ms1 / params()$population else ms <- ms1
  #     # annotation
  #     a <- list(
  #       x = h,
  #       y = ms,
  #       text = "75% adults fully vaccinated",
  #       xref = "x",
  #       yref = "y",
  #       showarrow = TRUE,
  #       arrowhead = 7,
  #       ax = -20,
  #       ay = -40
  #     )
  #   } else {
  #     a <- NULL # if herd immunity isn't achieved, don't provide annotation
  #   }
  # 
  #   plot_matrix1 <- i()$matrix
  #   vaccinations1 <- ts()
  #   protected1 <- i()$protected
  # 
  #   if (input$y_scale == "Percentage") {
  #     # convert to percentage of total population
  #     vaccines <- colnames(plot_matrix1)[((which(colnames(plot_matrix1) == "date")) + 1):
  #     ((which(colnames(plot_matrix1) == "total")) - 1)]
  #     # plot_matrix1[,c(vaccines, "total")] <- plot_matrix1[,c(vaccines, "total")] / params$population
  #     plot_matrix <- plot_matrix1 %>%
  #       mutate(across(!!vaccines[1]:total, ~ . / params()$population))
  #     vaccinations <- vaccinations1 %>%
  #       mutate(across(people_fully_vaccinated:people_vaccinated, ~ . / params()$population))
  #     protected <- protected1 %>%
  #       mutate(across(!!vaccines[1]:total, ~ . / params()$population))
  # 
  #     aged_80_plus <- params()$aged_80_plus / params()$population
  #     aged_70_79 <- params()$aged_70_79 / params()$population
  #     aged_60_69 <- params()$aged_60_69 / params()$population
  #     aged_50_59 <- params()$aged_50_59 / params()$population
  #     aged_20_49 <- params()$aged_20_49 / params()$population
  #     pop <- 1
  #     h_line_label <- "Country Population (Percentage)"
  #   } else {
  #     plot_matrix <- plot_matrix1
  #     vaccinations <- vaccinations1
  #     protected <- protected1
  #     aged_80_plus <- params()$aged_80_plus
  #     aged_70_79 <- params()$aged_70_79
  #     aged_60_69 <- params()$aged_60_69
  #     aged_50_59 <- params()$aged_50_59
  #     aged_20_49 <- params()$aged_20_49
  #     pop <- params()$population
  #     h_line_label <- "Country Population (Absolute)"
  #   }
  # 
  #   m <- list(size = 7, line = list(width = 0.7, color = "black"))
  #   p <- plot_ly(
  #     plot_matrix,
  #     x = ~date,
  #     y = ~total,
  #     type = "scatter",
  #     mode = "lines",
  #     name = "Fully  vaccinated - Airfinity forecasts" # ,
  #     # line = list(dash = 'dash')
  #   ) %>%
  #     add_trace(
  #       x = ~ vaccinations$date,
  #       y = ~ vaccinations$people_fully_vaccinated,
  #       type = "scatter",
  #       mode = "lines",
  #       name = "Fully vaccinated - actual" # ,
  #       # line = list(dash = 'line')
  #     ) %>%
  #     add_trace(
  #       x = ~ protected$date,
  #       y = ~ protected$total,
  #       type = "scatter",
  #       mode = "lines",
  #       name = "Protected - Airfinity forecasts"
  #     ) %>%
  #     add_markers(
  #       x = NULLtoNA(i()$aged_80_plus_vaccinated),
  #       y = aged_80_plus,
  #       name = "80+ Vaccinated",
  #       marker = m
  #     ) %>%
  #     add_markers(
  #       x = NULLtoNA(i()$aged_70_79_vaccinated),
  #       y = aged_70_79,
  #       name = "70+ Vaccinated",
  #       marker = m
  #     ) %>%
  #     add_markers(
  #       x = NULLtoNA(i()$aged_60_69_vaccinated),
  #       y = aged_60_69,
  #       name = "60+ Vaccinated",
  #       marker = m
  #     ) %>%
  #     add_markers(
  #       x = NULLtoNA(i()$aged_50_59_vaccinated),
  #       y = aged_50_59,
  #       name = "50+ Vaccinated",
  #       marker = m
  #     ) %>%
  #     add_markers(
  #       x = NULLtoNA(i()$aged_20_49_vaccinated),
  #       y = aged_20_49,
  #       name = "20+ Vaccinated",
  #       marker = m
  #     ) %>%
  #     add_segments(
  #       x = ~ min(date),
  #       xend = ~ max(date),
  #       y = pop,
  #       yend = pop,
  #       # line = list(dash = 'dash', width = 2, color = 'black'),
  #       line = list(dash = "dash", width = 1, color = "black"),
  #       name = h_line_label
  #     ) %>%
  #     layout(
  #       legend = list(
  #         orientation = "v"
  #       ),
  #       yaxis = list(
  #         title = "",
  #         fixedrange = FALSE # allows normal zoom when using rangeslider
  #       ),
  #       xaxis = list(
  #         title = ""
  #       ),
  #       annotations = a
  #     )
  # 
  #   if (input$y_scale == "Percentage") {
  #     p <- p %>%
  #       layout(yaxis = list(tickformat = ".0%"))
  #   }
  # 
  # 
  #   # ---------------
  #   # calculate trajectory of current vaccination programme - people fully vaccinated (if vaccination data available)
  #   if (nrow(vaccinations) > 0) {
  #     maxdate <- max(plot_matrix$date)
  #     vaccinations$daily_people_fully_vaccinated <- c(NA, diff(vaccinations$people_fully_vaccinated))
  # 
  #     vaccinations <-
  #       vaccinations %>%
  #       mutate(
  #         daily_people_fully_vaccinated = c(NA, diff(people_fully_vaccinated)),
  #         smoothed = zoo::rollmean(daily_people_fully_vaccinated, 7, fill = NA, align = "right")
  #       )
  # 
  #     dates <- seq.Date(from = vaccinations$date[nrow(vaccinations)], to = maxdate, by = "days")
  # 
  #     sv <- na.omit(vaccinations$smoothed) # smoothed vaccinations
  #     dv <- ifelse(length(sv) > 0, sv[length(sv)], 0) # latest smoothed vaccination count
  # 
  #     pv <- na.omit(vaccinations$people_fully_vaccinated)
  #     pv <- ifelse(length(pv) > 0, pv[length(pv)], 0)
  # 
  #     forecasts <-
  #       cbind.data.frame(
  #         date = dates,
  #         forecast = rep(dv, length(dates))
  #       ) %>%
  #       mutate(
  #         # cumulative_forecast = pmin(cumsum(forecast) + pv, params()$population)
  #         cumulative_forecast = pmin(cumsum(forecast) + pv, pop)
  #       )
  # 
  #     p <-
  #       p %>%
  #       add_trace(
  #         x = forecasts$date,
  #         y = forecasts$cumulative_forecast,
  #         type = "scatter",
  #         mode = "lines",
  #         name = "Actual - trajectory",
  #         line = list(
  #           color = "orange",
  #           dash = "dot"
  #         )
  #       ) %>%
  #       layout(hovermode = "x unified") %>%
  #       rangeslider(start = min(plot_matrix$date), end = as.Date("2021-12-31")) # hard code end date for range slider to be end 2021
  #   }
  # 
  #   return(p)
  # })
  # 
  # ## output plot of 1st dose vs second (i.e people vaccinated vs people fully vaccinated) ####
  # output$plot2 <- plotly::renderPlotly({
  #   req(i())
  #   if (sum(i()$matrix$vaccinated_75pc_adults) > 0) {
  #     ms1 <- i()$matrix$total[which(i()$matrix$vaccinated_75pc_adults == 1)[[1]]]
  #     h <- i()$vaccinated_75pc_adults
  #     if (input$y_scale == "Percentage") ms <- ms1 / params()$population else ms <- ms1
  # 
  #     # annotation
  #     a <- list(
  #       x = h,
  #       y = ms,
  #       text = "75% adults fully vaccinated",
  #       xref = "x",
  #       yref = "y",
  #       showarrow = TRUE,
  #       arrowhead = 7,
  #       ax = -20,
  #       ay = -40
  #     )
  #   } else {
  #     a <- NULL # if herd immunity isn't achieved, don't provide annotation
  #   }
  # 
  #   plot_matrix1 <- i()$matrix
  #   plot_people_vaccinated1 <- i()$people_vaccinated
  #   vaccinations1 <- ts()
  #   protected1 <- i()$protected
  # 
  #   if (input$y_scale == "Percentage") {
  #     # convert to percentage of total population
  #     vaccines <- colnames(plot_matrix1)[((which(colnames(plot_matrix1) == "date")) + 1):
  #     ((which(colnames(plot_matrix1) == "total")) - 1)]
  #     plot_matrix <- plot_matrix1 %>%
  #       mutate(across(!!vaccines[1]:total, ~ . / params()$population))
  #     plot_people_vaccinated <- plot_people_vaccinated1 %>%
  #       mutate(across(!!vaccines[1]:total, ~ . / params()$population))
  #     vaccinations <- vaccinations1 %>%
  #       mutate(across(people_fully_vaccinated:people_vaccinated, ~ . / params()$population))
  #     protected <- protected1 %>%
  #       mutate(across(!!vaccines[1]:total, ~ . / params()$population))
  # 
  #     pop <- 1
  #     h_line_label <- "Country Population (Percentage)"
  #   } else {
  #     plot_matrix <- plot_matrix1
  #     plot_people_vaccinated <- plot_people_vaccinated1
  #     vaccinations <- vaccinations1
  #     protected <- protected1
  # 
  #     pop <- params()$population
  #     h_line_label <- "Country Population (Absolute)"
  #   }
  # 
  #   m <- list(size = 7, line = list(width = 0.7, color = "black"))
  #   p <- plot_ly(
  #     plot_matrix,
  #     x = ~date,
  #     y = ~total,
  #     type = "scatter",
  #     mode = "lines",
  #     name = "Fully  vaccinated - Airfinity forecasts",
  #     color = I("#0047ba"),
  #     line = list(dash = "dash")
  #   ) %>%
  #     add_trace(
  #       y = ~ plot_people_vaccinated$total,
  #       name = "Part vaccinated - Airfinity forecasts",
  #       color = I("#7e56a3"),
  #       line = list(dash = "dash")
  #     ) %>%
  #     add_trace(
  #       x = ~ vaccinations$date,
  #       y = ~ vaccinations$people_fully_vaccinated,
  #       type = "scatter",
  #       mode = "lines",
  #       name = "Fully vaccinated - actual",
  #       color = I("#0047ba"),
  #       line = list(dash = "line")
  #     ) %>%
  #     add_trace(
  #       x = ~ vaccinations$date,
  #       y = ~ vaccinations$people_vaccinated,
  #       type = "scatter",
  #       mode = "lines",
  #       name = "Part vaccinated - actual",
  #       color = I("#7e56a3"),
  #       line = list(dash = "line")
  #     ) %>%
  #     add_segments(
  #       x = ~ min(date),
  #       xend = ~ max(date),
  #       y = pop,
  #       yend = pop,
  #       line = list(dash = "dash", width = 1, color = "black"),
  #       name = h_line_label
  #     ) %>%
  #     layout(
  #       legend = list(
  #         orientation = "v"
  #       ),
  #       yaxis = list(
  #         title = "",
  #         fixedrange = FALSE # allows normal zoom when using rangeslider
  #       ),
  #       xaxis = list(
  #         title = ""
  #       ),
  #       annotations = a,
  #       hovermode = "x unified"
  #     ) %>%
  #     rangeslider(start = min(plot_matrix$date), end = as.Date("2021-12-31")) # hard code end date for range slider to be end 2021
  # 
  #   if (input$y_scale == "Percentage") {
  #     p <- p %>%
  #       layout(yaxis = list(tickformat = "%"))
  #   }
  # 
  #   return(p)
  # })
  # 
  # ## Plot of Vaccine Supply ("Supply Forecast") (third plot in) ####
  # output$plot3 <- renderPlotly({
  #   req(input$country)
  #   req(i())
  # 
  #   if (!is.null(i())) {
  #     # o <- i()$matrix %>%
  #     o <- i()$supply_forecast %>%
  #       pivot_longer(cols = -date, names_to = "supplier", values_to = "supply")
  #     o$country <- params()$country
  #   } else {
  #     shiny::showNotification("Forecast unavailable", type = "warning")
  #     return(NULL)
  #   }
  # 
  #   if (input$y_scale == "Percentage") {
  #     o1 <- o %>%
  #       mutate(
  #         pop = params()$population,
  #         supply = (supply / pop) # * 100
  #       ) %>%
  #       select(-pop)
  #   } else {
  #     o1 <- o
  #   }
  # 
  #   wide <- o1 %>%
  #     pivot_wider(names_from = supplier, values_from = supply)
  # 
  #   d <- wide %>%
  #     select(-c(country, total)) %>%
  #     pivot_longer(-date)
  # 
  #   p <- plot_ly(
  #     d,
  #     x = ~date,
  #     y = ~value,
  #     type = "scatter",
  #     mode = "none",
  #     stackgroup = "one",
  #     color = ~name
  #   ) %>%
  #     layout(
  #       legend = list(
  #         orientation = "v"
  #       ),
  #       yaxis = list(
  #         title = "",
  #         fixedrange = FALSE # allows normal zoom when using rangeslider
  #       ),
  #       xaxis = list(
  #         title = ""
  #       )
  #     ) %>%
  #     rangeslider(start = min(d$date), end = as.Date("2021-12-31")) # hard code end date for range slider to be end 2021
  # 
  #   if (input$y_scale == "Percentage") {
  #     p <- p %>%
  #       layout(yaxis = list(tickformat = "%"))
  #   }
  # 
  #   return(p)
  # })
  # 
  # ## output plot of daily infections/deaths etc ####
  # output$plot4 <- renderPlotly({
  #   req(ts())
  # 
  #   o1 <- ts()
  #   o <- o1
  # 
  #   p1 <- plot_ly(
  #     o,
  #     x = ~date,
  #     y = ~new_cases_smoothed,
  #     type = "scatter",
  #     mode = "lines",
  #     line = list(color = "mediumpurple"),
  #     name = "New infections (smoothed)"
  #   ) %>%
  #     add_lines(
  #       y = ~hosp_patients,
  #       name = "Hospital patients",
  #       line = list(color = "darkslateblue"),
  #     ) %>%
  #     rangeslider(start = Sys.Date() - 90, end = Sys.Date())
  # 
  #   p2 <-
  #     plot_ly(
  #       o,
  #       x = ~date,
  #       y = ~new_deaths_smoothed,
  #       type = "scatter",
  #       mode = "lines",
  #       line = list(color = "skyblue"),
  #       name = "New deaths (Smoothed)"
  #     ) %>%
  #     add_lines(
  #       y = ~icu_patients,
  #       name = "ICU patients",
  #       line = list(color = "steelblue")
  #     ) %>%
  #     rangeslider(start = Sys.Date() - 90, end = Sys.Date())
  # 
  #   subplot(p1, p2, shareY = FALSE) %>% layout(legend = list(orientation = "h"), hovermode = "x unified")
  # })
  # 
  # # ===============
  # # downloads
  # # ===============
  # 
  # # actual vs expected vaccinations, and protected
  # download_df1 <- reactive({
  #   req(i())
  #   req(ts())
  # 
  #   forecast_dat <- i()$matrix %>%
  #     select(date, fully_vaccinated_forecast = total)
  #   obs_dat <- ts() %>%
  #     select(date, fully_vaccinated_observed = people_fully_vaccinated)
  #   protected_dat <- i()$protected %>%
  #     select(date, people_protected = total)
  # 
  #   # join them together!
  #   dat <- forecast_dat %>%
  #     left_join(obs_dat, by = c("date")) %>%
  #     left_join(protected_dat, by = c("date")) %>%
  #     mutate(
  #       across(!contains("date"), ~ round(.x)),
  #       country = !!input$country
  #     ) %>%
  #     relocate(country)
  # 
  #   return(dat)
  # })
  # 
  # # people vaccinated and people fully vaccinated, observed and forecasted
  # download_df2 <- reactive({
  #   req(i())
  #   req(ts())
  # 
  #   full_forecast_dat <- i()$matrix %>%
  #     select(date, fully_vaccinated_forecast = total)
  #   partial_forecast_dat <- i()$people_vaccinated %>%
  #     select(date, people_vaccinated_forecast = total)
  #   obs_dat <- ts() %>%
  #     select(date, people_vaccinated_observed = people_vaccinated, fully_vaccinated_observed = people_fully_vaccinated)
  # 
  #   # join them together!
  #   dat <- full_forecast_dat %>%
  #     left_join(partial_forecast_dat, by = "date") %>%
  #     left_join(obs_dat, by = c("date")) %>%
  #     select(sort(tidyselect::peek_vars())) %>%
  #     mutate(
  #       across(!contains("date"), ~ round(.x)),
  #       country = !!input$country
  #     ) %>%
  #     relocate(country)
  # 
  #   return(dat)
  # })
  # 
  # # supply forecasts
  # download_df3 <- reactive({
  #   req(i())
  # 
  #   dat <- i()$supply_forecast %>%
  #     # mutate(across(!contains("date"), ~plyr::round_any(.x, 1000))) # round to nearest 1000
  #     mutate(
  #       across(!contains("date"), ~ round(.x)), # round to nearest whole number
  #       country = !!input$country
  #     ) %>%
  #     relocate(country)
  # 
  #   return(dat)
  # })
  # 
  # # other country stats
  # download_df4 <- reactive({
  #   req(ts())
  # 
  #   o1 <- ts()
  #   o <- o1
  # 
  #   dat <- o %>%
  #     select(
  #       country = location, date,
  #       new_cases_smoothed, hosp_patients, # plot 1
  #       new_deaths_smoothed, icu_patients
  #     ) # plot 2
  # 
  #   return(dat)
  # })
  # 
  # 
  # 
  # # downloads
  # output$download_plot1 <- downloadHandler(
  #   filename = function() {
  #     paste("download.csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(download_df1(), file, row.names = FALSE)
  #   }
  # )
  # output$download_plot2 <- downloadHandler(
  #   filename = function() {
  #     paste("download.csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(download_df2(), file, row.names = FALSE)
  #   }
  # )
  # output$download_plot3 <- downloadHandler(
  #   filename = function() {
  #     paste("download.csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(download_df3(), file, row.names = FALSE)
  #   }
  # )
  # 
  # output$download_plot4 <- downloadHandler(
  #   filename = function() {
  #     paste("download.csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(download_df4(), file, row.names = FALSE)
  #   }
  # )
}
