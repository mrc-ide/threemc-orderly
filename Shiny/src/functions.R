#### common functions amongst plots

#### reads in results ####
results_reader <- function(cntry = NULL, type, dir_path, pattern = NULL,
                           model_type = NULL, ...) {
  # do we want results for discrete ages or "binned" age groups?
  if (type == "age") ages <- "_Age_" else ages <- "AgeGroup"
  # list files, and pull names for desired country and type
  # (maybe shouldn't hard code this path into the function?)
  f <- list.files(dir_path, full.names = T)
  f <- f[grepl(ages, f)]
  if (!is.null(cntry)) f <- f[grepl(cntry, f)]
  if (!is.null(pattern)) f <- f[grepl(pattern, f)]

  if (length(f) < 3) message("some data is missing!")
  if (length(f) > 3) message("some unexpected data may be present!")

  # load files and append them together. Return this appended df
  results_age <- as.data.frame(data.table::rbindlist(
    lapply(f, fread, ...), use.names = T
  ))
  if (!is.null(model_type)) {
    results_age <- results_age %>%
      filter(model %in% model_type)
  }
  # Avoid annoying error with model type created when aggregating
  if (sum(grepl("programme", results_age$model)) != 0) {
    results_age <- results_age %>%
      mutate(model = stringr::str_replace(model, "programme", "program"))
  }
  return(results_age)
}

#### Function to Add "Light" Column to Data ####
add_last_surveys <- function(.data, last_surveys, add_rows = TRUE, join_cols = c("iso3", "area_id")) {
    # join data with last_surveys
    if (!"iso3" %in% names(.data)) {
        .data$iso3 <- substr(.data$area_id, 0, 3)
    }
    .data <- .data %>%
        # add last surveys for each country
        # left_join(last_surveys, by = c("iso3", "area_id")) %>%
        left_join(last_surveys, by = join_cols) %>%
        # assume missing survey years are max for that country
        group_by(iso3) %>%
        mutate(survey_year = ifelse(
            is.na(survey_year),  max(survey_year, na.rm = TRUE), survey_year
        )) %>%
        ungroup() %>%
        # add alpha to plot if the circumcisions are projected
        mutate(light = ifelse(year < survey_year, "Surveyed", "Projected"))

    if (add_rows == TRUE) {
        # add additional row for survey year with light == "Surveyed" (avoids gaps in plot)
        extra_rows <- .data %>%
            filter(year == survey_year) %>%
            mutate(light = "Surveyed")
        .data <- rbind(.data, extra_rows)
    }

    return(.data %>% arrange(area_id, year, type, age_group))
}


# function to change age_group convention from "Y0x_0y" to "x - y"
change_agegroup_convention <- function(.data) {

    lower <- as.numeric(substr(.data$age_group, 3, 4))
    if (all(!is.na(as.numeric(lower)))) {
        upper <- as.numeric(substr(.data$age_group, 7, 8))
        .data$age_group <- paste(lower, upper, sep = "-")
    }
    return(.data)
}

#### appends summary column names with letter of choice ####
summary_col_renamer <- function(.data, letter = "x") {
  .data <- .data %>%
    rename_with(.fn = ~paste0(., ".", letter), .cols = mean:upper)

  return(.data)
}

#### order area name by `area_sort_order` ####
order_area_name <- function(.data, areas = NULL) {

  # add area_sort_order column to the data, if not already present
  if (!"area_sort_order" %in% names(.data) & is.null(areas)) {
      stop(paste0("please provide either .data or areas with an ",
                  "'area_sort_order' column"))
  }

  if (!"area_sort_order" %in% names(.data) & !is.null(areas)) {
    # remove sf class, if required
    if (inherits(areas, "sf")) areas <- st_drop_geometry(areas)

    # pull relevant areas columns
    areas <- areas %>%
      dplyr::select(area_id, area_name, area_sort_order)

    # join area_sort_order into data
    .data <- .data %>%
      select(-matches("area_name")) %>%
      left_join(areas, by = c("area_id"))
  }
  # get age column present in data
  age_column <- c("age", "age_group")
  age_column <- age_column[age_column %in% names(.data)]

  # order by area_sort_order, year and age
  .data %>%
    dplyr::arrange(area_sort_order, year, age_column)
}

#### add area information ####
# Add useful columns from `areas` into data
add_area_info <- function(.data, areas, area_names_from = "areas") {

  if ("sf" %in% class(areas)) areas <- st_drop_geometry(areas)
  areas_join <- areas %>%
    dplyr::select(
      iso3,       area_id,          area_name,
      area_level, area_level_label, area_sort_order
    ) %>%
    distinct()

  # where do we want the area_names column to come from?
  if (area_names_from == "data") {
    areas_join <- select(areas_join, - matches("^area_name"))
  } else if (area_names_from == "areas") {
    .data <- select(.data, -matches("^area_name"))
  }

  left_join(.data, areas_join)
}

#### split_area_level ####
# split results by area level (and optionally by year), and then again so
# only the number of plots desired on each page is displayed
split_area_level <- function(.data, years = FALSE, n_plots = NULL) {

  # find unique areas. Only want "n_plots" areas on each page of the plot
  if (!is.null(n_plots)) {
      distinct_areas <- .data %>%
          distinct(area_id, area_level) %>%
          group_by(area_level) %>%
          dplyr::mutate(split = ceiling(row_number() / n_plots)) %>%
          ungroup()
      .data <- .data %>%
          left_join(distinct_areas, by = c("area_id", "area_level"))
  }

  # split by area level
  .data <- split(.data, .data$area_level)

  # Split options:
  # 1. split by year and number of regions to show on each plot (n_plot) desired
  # 2. split by year but not by n_plot
  # 3. split by n_plot but not year
  # 4. split by neither year nor n_plot

  if (years == TRUE & !is.null(n_plots)) { # 1.
    .data <- lapply(.data, function(x) split(x, x$year))
    lapply(.data, function(x) {
      lapply(x, function(y) {
        split(y, y$split)
      })
    })
  } else if (years == TRUE & is.null(n_plots)) { # 2.
      lapply(.data, function(x) split(x, x$year))
  } else if (years == FALSE & !is.null(n_plots)) { # 3.
      lapply(.data, function(x) split(x, x$split))
  } else { # 4.
      return(.data)
  }
}

#### Plots from paper ####
# figure 1 (coverage and number of circumcisions performed)
# if str_save is left blank, just return the plot
plt_mc_coverage_prevalence <- function(
  results_agegroup, areas, spec_age_group, spec_years, area_levels, spec_model,
  main = NULL, str_save = NULL, save_width, save_height, n_plots = 1
  ) {

  if (length(spec_years) == 2) {
    spec_years <- spec_years[1]:last(spec_years)
  }

  # Subsetting
  tmp1 <- results_agegroup %>%
    filter(
      # type %in%     c("MMC-nTs performed", "MMC-Ts performed", "TMCs performed"),
      type %in% c("MMCs performed", "TMCs performed"),
      # age_group ==  "10+",
      age_group == spec_age_group,
      # year %in%     c(2009:2021),
      year %in%       spec_years,
      area_level %in% area_levels,
      model == spec_model
    ) %>%
    # rename columns
    summary_col_renamer()

  tmp2 <- results_agegroup %>%
    filter(
      # type %in% c("MMC-nT coverage", "MMC-T coverage", "TMC coverage"),
      type %in% c("MMC coverage", "TMC coverage"),
      age_group == spec_age_group,
      year %in% spec_years,
      area_level %in% area_levels,
      model == spec_model
    ) %>%
    summary_col_renamer(letter = "y")


  # Relabelling for plot
  plot_relabler <- function(.data) {
    .data %>%
      mutate(
        # type = ifelse(type %like% "MMC-nT", "MMC-nT",
        #               ifelse(type %like% "MMC-T", "MMC-T", "TMC"))
        type = ifelse(grepl("MMC", type), "MMC", "TMC")
      )
  }
  tmp1 <- plot_relabler(tmp1)
  tmp2 <- plot_relabler(tmp2)

  # Adding labels to the facet
  tmp1$test <- "B"
  tmp2$test <- "A"

  # Appending datasets together
  tmp <- bind_rows(tmp1, tmp2)

  # add in required columns from areas and order area names (make function)
  tmp <- add_area_info(tmp, areas)

  # Dummy dataset for limits in each panel
  dummy1 <- rbind(expand.grid(x = c(first(spec_years), last(spec_years)),
                              y = c(0, 100),
                              type = NA,
                              test = "A"),
                  expand.grid(x = c(2009, 2021),
                              y = c(0, 600),
                              type = NA,
                              test = "B"))

  # Dummy dataset to add 90% target
  if (spec_age_group == "15-49") {
    target <- 80
  } else {
    target <- 90
  }
  dummy2 = data.frame("test" = c("A", "B"),
                      "type" = NA,
                      "y"    = c(target, NA))

  # split tmp by area level and number of desired plots
  tmp <- split_area_level(tmp, n_plots = n_plots)

  plots <- lapply(tmp, function(a) {
    lapply(a, function(b) {

      # title displaying area level and label
      spec_title <- paste(
        b$iso3[1],
        b$area_name[1],
        paste0("Area Level: ", b$area_level[1]),
        b$area_level_label[1],
        paste0(b$age_group[1], " years old"),
        sep = ", "
      )

      # add additional text to title, if desired
      if (!is.null(main)) {
          spec_title <- paste(main, spec_title)
      }

      ggplot(b,
             aes(x = year,
                 group = type,
                 fill = type)) +
        # Adding fake limits to the plot
        geom_point(data = dummy1,
                   aes(x = x,
                       y = y),
                   colour = NA) +
        # Adding target line to prevalence
        geom_hline(data = dummy2,
                   aes(yintercept = y),
                   size = 2,
                   linetype = "dashed",
                   colour = "grey50") +
        # Prevalence as area plot
        geom_area(aes(y = 100 * mean.y)) +
        # Number of MCs performed as bar chart
        # geom_bar(aes(y = mean.x/1000),
        geom_bar(aes(y = mean.x),
                 stat = "identity") +
        # Setting for the axes
        scale_x_continuous(
            breaks = seq(first(spec_years), last(spec_years), by = 1)
        ) +
        scale_y_continuous(
            breaks = scales::pretty_breaks(8),
            # breaks = scales::label_comma(),
            limits = c(0, NA)
        ) +
        # Setting colour palette
        scale_fill_manual(
          values = wesanderson::wes_palette("Zissou1", 3)[c(1, 3)]
        ) +
        # Plotting labels
        labs(x = "Year",
             y = "",
             fill = "") +
        # Minimal theme
        theme_bw() +
        ggtitle(spec_title) +
        # Facet wrapping
        facet_wrap(vars(test),
                   strip.position = "left",
                   scales = "free",
                   labeller = as_labeller(c(
                     A = "Circumcision coverage (%)",
                     # B = "Number of circumcisions performed (in 1000s)"))) +
                     B = "Number of circumcisions performed"))) +
        # facet_wrap again by area_name
        # facet_wrap(~ area_name) +
        # Extra options on the plot
        theme(axis.text = element_text(size = 15),
              strip.text = element_text(size = 16),
              strip.background = element_blank(),
              panel.grid.minor = element_blank(),
              # panel.grid.major = element_blank(),
              legend.text = element_text(size = 16),
              axis.title = element_text(size = 18),
              plot.title = element_text(size = 26, hjust = 0.5),
              axis.text.x = element_text(
                # size = 12,
                hjust = 1,
                vjust = 1,
                angle = 45
              ),
              strip.placement = "outside",
              legend.position = "bottom")
    })
  })

  if (!is.null(str_save)) {
    ggsave(
      # filename = paste0("Runs/plots/", cntry, "_Figure1.pdf"),
      filename = str_save,
      plot = gridExtra:: marrangeGrob(rlang::squash(plots), nrow = 1, ncol = 1),
      dpi = "retina",
      # width = 16,
      width = save_width,
      # height = 7.5
      height = save_height
    )
  } else {
    return(rlang::squash(plots))
  }
}

# fig 2 (circumcision coverage, split by type, across different discrete ages)
plt_age_coverage_by_type <- function(
  results_age, areas, spec_years, area_levels, spec_model, spec_ages = c(0, 60),
  main = NULL, str_save = NULL, save_width, save_height, n_plots = 1
) {

  # Subsetting
  tmp1 <- results_age %>%
    filter(
      type %in%     c("MC probability", "MMC probability", "TMC probability",
                      "MC coverage",    "MMC coverage",    "TMC coverage"),
      # year %in%     c(2009, 2015, 2021)# ,
      year %in% spec_years,
      area_level %in% area_levels,
      age %in% seq(spec_ages[1], spec_ages[2], by = 1),
      model == spec_model
    ) %>%
    # rename columns
    summary_col_renamer() %>%
    dplyr::mutate(
      # multiply prevalence by 100
      across(c(mean.x, lower.x, upper.x), ~ ifelse(grepl("coverage", type),
                                                   . * 100,
                                                   .)),
      # relabel for plot
      type1 = ifelse(grepl("coverage", type), "Y", "Z"),
      type2 = ifelse(grepl("MMC", type), "B",
                     ifelse(grepl("TMC", type), "C", "A")) # A for MC
    )

  # Dummy dataset for limits in each panel
  dummy1 <- rbind(expand.grid(x = spec_ages,
                              y = c(0, 0.2),
                              year = NA,
                              type2 = c("A", "B", "C"),
                              type1 = "Z"),
                  expand.grid(x = spec_ages,
                              y = c(0, 100),
                              year = NA,
                              type2 = c("A", "B", "C"),
                              type1 = "Y"))

  # Dummy dataset to add 90% target
  dummy2 = data.frame(type2 = c("A", "B", "C"),
                      type1 = "Y",
                      year = NA,
                      y = c(90, 90, 90))

  # add in required columns from areas and order area names
  tmp1 <- add_area_info(tmp1, areas)

  # split tmp by area level and number of desired plots
  tmp1 <- split_area_level(tmp1, n_plots = n_plots)

  plots <- lapply(tmp1, function(a) {
    lapply(a, function(b) {

      # title displaying area level and label
      spec_title <- paste(
        b$iso3[1],
        b$area_name[1],
        paste0("Area Level: ", b$area_level[1]),
        b$area_level_label[1],
        sep = ", "
      )
      if (!is.null(main)) spec_title <- paste(main, spec_title)
      ggplot(b,
             aes(x = age,
                 group = as.factor(year),
                 fill = as.factor(year),
                 colour = as.factor(year))) +
        # Adding fake limits to the plot
        geom_point(data = dummy1,
                   aes(x = x,
                       y = y),
                   colour = NA) +
        # Adding target line to prevalence
        geom_hline(data = dummy2,
                   aes(yintercept = y),
                   size = 2,
                   linetype = "dashed",
                   colour = "grey50") +
        # Prevalence as area plot
        geom_ribbon(aes(ymin = lower.x,
                        ymax = upper.x),
                    colour = NA,
                    alpha = 0.5) +
        # Prevalence as area plot
        geom_line(aes(y = mean.x),
                  size = 1) +
        # Setting for the axes
        scale_x_continuous(breaks = seq(spec_ages[1], spec_ages[2], by = 5)) +
        scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(0, NA)) +
        # Setting colour palette
        scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)) +
        scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)) +
        # Plotting labels
        ggtitle(spec_title) +
        labs(x = "Age",
             y = "",
             colour = "",
             fill = "") +
        # Minimal theme
        theme_bw() +
        # Facet wrapping
        facet_grid(type1 ~ type2,
                   scales = "free",
                   labeller = as_labeller(c(
                     A = "Total",
                     B = "Medical",
                     C = "Traditional",
                     Y = "Circumcision coverage (%)",
                     Z = "Annual probability of circumcision")),
                   switch = "y") +
        # Extra options on the plot
        theme(axis.text = element_text(size = 16),
              strip.text = element_text(size = 16),
              panel.grid = element_blank(),
              strip.background = element_blank(),
              legend.text = element_text(size = 16),
              axis.title = element_text(size = 18),
              plot.title = element_text(size = 26, hjust = 0.5),
              strip.placement = "outside",
              legend.position = "bottom")
    })
  })

  if (!is.null(str_save)) {
    ggsave(
      # filename = paste0("Runs/plots/", cntry, "_Figure2.pdf"),
      filename = str_save,
      plot = gridExtra:: marrangeGrob(rlang::squash(plots), nrow = 1, ncol = 1),
      dpi = "retina",
      # width = 15,
      width = save_width,
      # height = 11
      height = save_height
    )
  } else {
    return(rlang::squash(plots))
  }
}

# Plot Map (fig 3). Works for multiple or single countries. Multiple can be either as
# a single plot of SSA (only way currently supported), or faceted by country
plt_coverage_map <- function(
    results_agegroup, areas, colourPalette,
    spec_age_group, spec_years, spec_model,
    plot_type, # = c("single", "map", "split"),
    spec_main_title = NULL,
    spec_countries = NULL, results_area_level = NULL, country_area_level = NULL,
    inc_difference = FALSE,
    str_save = NULL, save_width, save_height, n_plots = 1
) {

    # To Do:
    # 1. Maybe add warning if there are missing iso3 codes?
    # 2. Get working for faceted plots for all/several countries (use cowplot)

    # if we just want to look at a single country
    if (plot_type == "single" & is.null(spec_countries)) {
        if(is.null(spec_countries)) {
            stop("Please specify an iso3 code")
        } else if (length(spec_countries) > 1) {
            stop("Please specify only one iso3 code")
        }
    }

  if (!"iso3" %in% names(results_agegroup)) {
    results_agegroup$iso3 <- substr(results_agegroup$area_id, 0, 3)
  }

  # filter for desired country
  if (!is.null(spec_countries)) {
    areas <- areas %>%
      filter(iso3 %in% spec_countries)
    results_agegroup <- results_agegroup %>%
      filter(grepl(paste(spec_countries, collapse = "|"), area_id))
    if (nrow(results_agegroup) == 0) {
      stop ("No records present for given iso3 code(s)")
    }
  }

    # take only required columns in areas for later joining with results
    areas_join <- areas %>%
        dplyr::select(iso3, area_id, area_name, area_level)

    # Subsetting results
    if (!is.null(results_area_level)) {
        results_agegroup <- results_agegroup %>%
            filter(area_level == results_area_level)
    } else {
      results_agegroup <- results_agegroup %>%
        group_by(iso3) %>%
        filter(area_level == max(area_level)) %>%
        ungroup()
    }
    tmp <- results_agegroup %>%
        filter(
            area_id != "",
            year %in%       spec_years,
            age_group ==    spec_age_group,
            # area_level <=   results_area_level,
            model ==        spec_model,
            type %in%       c("MC coverage", "MMC coverage", "TMC coverage")
        ) %>%
        select(-matches("area_name")) %>%
        # Merging to shapefiles
        left_join(areas_join) %>%
        # filter out areas with missing iso3, which cause errors with below
        filter(!is.na(iso3)) %>%
        # take maximum area level for known regions
        group_by(iso3) %>%
        filter(area_level == max(area_level, na.rm = T)) %>%
        ungroup() %>%
        # Altering labels for the plot
        dplyr::mutate(
            type = ifelse(grepl("MMC", type), "Medical",
                          ifelse(grepl("TMC", type), "Traditional", "Total"))
        ) %>%
        # change data to sf object
        st_as_sf()

    # filter overlaying area shapes for specified area level
    areas_plot <- areas
    if (!is.null(country_area_level)) {
        areas_plot <- areas_plot %>%
            filter(area_level == country_area_level)
    }

    # repair polygons which may be invalid
    tmp <- st_make_valid(tmp)
    areas_plot <- st_make_valid(areas_plot)

    # if (plot_type != "single") {
    #     # if bb_box is too far West (likely due to Cabo Verde(?)), adjust
    #     # could have plot dimensions as input??
    #     bb <- st_bbox(areas_plot)
    #     if (bb[[1]] < -20) {
    #         areas_plot <- st_crop(areas_plot, xmin = -20, ymin = bb[[2]],
    #                               xmax = bb[[3]], ymax = bb[[4]])
    #     }
    # }

    # code for an individual plot:
    map_plot <- function(spec_results, spec_areas, colourPalette) {

        p <- ggplot(spec_results) +
            geom_sf(aes(fill = mean),
                    size = 0.5,
                    colour = NA) +
            geom_sf(data = spec_areas,
                    colour = "black",
                    size = 0.5,
                    fill = NA) +
            labs(fill = "") +
            scale_fill_gradientn(colours = colourPalette,
                                 breaks = seq(0, 1, by = 0.1),
                                 limits = c(0, 1),
                                 label = scales::label_percent(accuracy = 1),
                                 guide = guide_colourbar(label = TRUE,
                                                        draw.ulim = TRUE,
                                                        draw.llim = TRUE,
                                                        frame.colour = "black",
                                                        ticks = TRUE,
                                                        barheight = 1,
                                                        barwidth = 30)) +
            theme_minimal() +
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  strip.text = element_text(size = 20, face = "bold"),
                  legend.text = element_text(size = 14, face = "bold"),
                  plot.title = element_text(size = 26, hjust = 0.5),
                  panel.grid = element_blank(),
                  legend.position = "bottom")

        # facet wrap differently according to "type":
        # if we have only one type, facet wrap by year
        if (length(unique(spec_results$type)) == 1) {
            p <- p + facet_wrap(~ year)
            # for multiple types, facet_wrap by both type and year
        } else {
            p <- p + facet_wrap(type ~ year)
        }
    }

    # Add difference, if specified
    tmp$year <- as.factor(tmp$year)
    if (inc_difference == TRUE) {

      # browser()
      diff_df <- tmp %>%
        arrange(year) %>%
        group_split(area_id, model, type, age_group) # %>%
        # purrr::map(~ slice(
        #   # .data = mutate(.x, across(mean:upper, ~ max(0, diff(.)))),
        #   .data = mutate(
        #       .x,
        #       across(mean:upper, ~ ifelse(
        #           type == "Traditional", min(0, -diff(.)), max(0, diff(.))
        #       ))
        #   ),
        #   n()
        # )) %>%
     diff_df <- lapply(diff_df, function(x) {
         if (all(x$type == "Traditional")) x <- x[nrow(x):1, ]
         x <- slice(
             mutate(x, across(mean:upper, ~ max(0, diff(.)))),
             n()
         )
     }) %>%
        bind_rows() %>%
        mutate(
          # year = "% Change"
          year = ifelse(type == "Traditional", "-% Change"  ,"% Change")
        )

      tmp <- bind_rows(tmp, diff_df) %>%
          mutate(year = factor(
              year, levels = c(spec_years, "% Change", "-% Change"))
          )
    }

    # split by type
    tmp1 <- split(tmp, tmp$type)
    if (plot_type == "split") {
        # split by type and country
        tmp1 <- lapply(tmp1, function(x) split(x, x$iso3))
    }
    # order appropriately
    tmp1 <- tmp1[c(which(names(tmp1) == "Total"),
                   which(names(tmp1) == "Medical"),
                   which(names(tmp1) == "Traditional"))]

    # for plotting an individual country:
    if (plot_type == "single") {
        areas_plot <- areas_plot %>%
            filter(iso3 %in% tmp$iso3)

        plots <- lapply(tmp1, function(x) {

            # if (is.null(spec_main_title)) {
            # area_name <- x %>%
            #     filter(area_level == min(area_level)) %>%
            #     pull(area_name) %>%
            #     first()
            #   # spec_main_title_single <- paste(x$iso3[1],
            #   spec_main_title_single <- paste(area_name,
            #                                   x$type[1],
            #                                   x$age_group[1],
            #                                   "years old")
            # } else {
            #   spec_main_title_single <- spec_main_title
            # }

            if (is.null(spec_main_title)) spec_main_title_single <- x$iso3[1]
            spec_main_title_single <- paste(spec_main_title,
                                            x$type[1],
                                            x$age_group[1],
                                            "years old")

            # if (all(x$type == "Traditional")) browser()
            # ensure change in percentage is last
            if (inc_difference == TRUE) {

              change <- "% Change"
              if (all(x$type == "Traditional")) change <- paste0("-", change)

              # ensure facet order
              x$year <- factor(
                x$year,
                levels = c(
                  spec_years[1],
                  spec_years[2],
                  # "% Change",
                  change
                )
              )
            }

            map_plot(x, areas_plot, colourPalette) +
                ggtitle(spec_main_title_single)
        })

        # insert something to join these plots here!

    } else if (plot_type == "map") {
        # create plot for each specific type

        plots <- lapply(tmp1, function(x) {
            # change year to factor, to change facet titles
            if (is.null(spec_main_title)) {
              x$year <- factor(x$year,
                               labels = paste(unique(x$type),
                                              "Circumcision",
                                              unique(x$year),
                                              "-",
                                              unique(x$age_group),
                                              "year olds"))
            } else {
              x$year <- factor(
                x$year,
                levels = c(
                  spec_years[1],
                  spec_years[2],
                  # paste0("% Change, ", spec_years[1], "-", last(spec_years))
                  "% Change"
                )
              )
            }
            map_plot(x, areas_plot, colourPalette)
        })

        if (!is.null(spec_main_title)) {
          plots <- lapply(plots, function(x) {
            x + ggtitle(spec_main_title)
          })
        }
    } else if (plot_type == "split") {

        plots <- lapply(tmp1, function(a) {
            lapply(a, function(b) {
                spec_areas <- filter(areas_plot, iso3 %in% b$iso3)
                p <- map_plot(b, spec_areas, colourPalette)
                # add title to plots
                if (is.null(spec_main_title)) {
                  spec_main_title_split <- paste(
                    b$iso3[1],
                    ifelse(b$type[1] == "Total", "MC",
                           ifelse(b$type[1] == "Medical",
                                  "MMC", "TMC")),
                    "-",
                    b$age_group[1],
                    "years old"
                  )
                  p +
                    ggtitle(spec_main_title_split)
              } else return(p)
            })
        })

        # shared legend/colourbar for plots
        legend <- cowplot::get_legend(plots[[1]][[1]])
        # number of countries to plot for
        n_cntry <- length(plots[[1]])
        # dimensions of cowplot (user inputted)
        square_root <- sqrt(n_plots)
        if (round(square_root) != square_root) {
            message("n_plots not square number, plot number may differ")
        }
        n_row <- floor(square_root)
        n_col <- ceiling(square_root)
        n_plot <- n_row * n_col

        # number of loops required to make plots
        n_loops <- ceiling(n_cntry / n_plot)

        # do for each "type" plot
        for (i in seq_along(plots)) {
            n <- 1 # initialise
            # first plot will be 1:3, then 4:6, 7:9, 10:12, 13
            plots2 <- vector(mode = "list", length = n_loops)

            for (j in seq_len(n_loops)) {
                plot_n <- n : (n + n_plot - 1)
                plot_n <- plot_n[plot_n <= n_cntry]

                spec_plots <- plots[[i]][plot_n]
                plots2[[j]] <- cowplot::plot_grid(
                    plotlist = lapply(spec_plots, function(x) {
                        x + theme(legend.position = "none",
                                  strip.text = element_text(size = 15))
                    }), nrow = n_row, ncol = n_col, scale = 0.85)
                plots2[[j]] <- plots2[[j]] + cowplot::draw_grob(legend, vjust = 0.48)
                # plots2[[j]] <- cowplot::plot_grid(title, plots2[[j]],
                #                                   ncol = 1,
                #                                   rel_heights = c(0.1, 1))
                n <- n + n_plot
            }
            plots[[i]] <- plots2
        }
        plots <- rlang::squash(plots)

        if (!is.null(spec_main_title)) {
          plots <- lapply(plots, function(x) {
            x + ggtitle(spec_main_title)
          })
        }
    }

    # If desired, save plots, else, return them ("ungrobbed")
    if (!is.null(str_save)) {
        plots <- gridExtra::marrangeGrob(plots, nrow = 1, ncol = 1)
        ggsave(filename = str_save,
               plot = plots,
               dpi = "retina",
               width = 15,
               height = 11)
    } else {
        return(plots)
    }
}


# Plot MC coverage by year, split by type (figure 4)
plt_area_facet_coverage <- function(
    results_agegroup,
    areas,
    area_order = NULL,
    hor_line = 90,
    spec_years = 2008:2020,
    spec_age_group = "10-29",
    area_levels = unique(results_agegroup$area_level),
    province_split = FALSE,
    spec_model = "No program data",
    str_save = NULL,
    # spec_title = paste0("Male Circumcision Coverage, ",
    spec_title = paste0("Coverage, ",
                        spec_years[1], "-", last(spec_years),
                        ", ages ", spec_age_group),
    save_width = 24,
    save_height = 21,
    n_plots = 12
) {

    # Subsetting
    tmp <- results_agegroup %>%
        filter(
            type %in% c("MC coverage", "MMC coverage", "TMC coverage"),
            age_group == spec_age_group,
            year %in% spec_years,
            area_level %in% area_levels,
            model == spec_model
        ) %>%
        summary_col_renamer(letter = "y") %>%
        # Relabelling for plot
        mutate(
            # type = stringr::str_remove(type, " coverage")# ,
            # area_id = factor(area_id, unique(.data$area_id))
            type = ifelse(grepl("MMC", type),
                          # "Medical Male Circumcision (MMC)",
                          "MMC",
                          ifelse(grepl("TMC", type),
                                 # "Traditional Male Circumcision (TMC)",
                                 "TMC",
                                 # "Medical Circumcision (MC)")),
                                 "MC")),
            area_name = ifelse(grepl("Tanzania", area_name),
                               "Tanzania",
                               area_name)
        )

    # reorder panels geographically
    if (!is.null(area_order)) {
        ordered_areas <- distinct(tmp, area_id, area_name)
        ordered_areas$area_id <-  factor(
            ordered_areas$area_id,
            # levels = c("BFA", "TGO", "AGO", "ETH", "KEN", "TZA", "UGA", "RWA",
            #            "MOZ", "MWI", "ZMB", "ZWE", "NAM", "ZAF", "SWZ", "LSO")
            levels = area_order
        )
        ordered_areas <- ordered_areas[order(ordered_areas$area_id), ]
        tmp$area_name <- factor(
            tmp$area_name,
            levels = ordered_areas$area_name
        )
    }

    # add in required columns from areas and order area names (make function)
    tmp <- add_area_info(tmp, areas)

    # data for specific types of circumcision, not total
    tmp1 <- tmp %>%
        # filter(type == "Medical Circumcision (MC)")
        filter(type == "MC")
    tmp <- tmp %>%
        filter(type != "MC") %>%
        plyr::rbind.fill()

    # split by area level and number of desired plots
    if (province_split) {
      # if desired, split by parent area id instead of by n_plots
      tmp <- dplyr::group_split(tmp, area_level) %>%
          purrr::map(~ dplyr::group_split(.x, parent_area_id))
      tmp1 <- dplyr::group_split(tmp1, area_level) %>%
          purrr::map(~ dplyr::group_split(.x, parent_area_id))
    } else {
      tmp <- split_area_level(tmp, n_plots = n_plots)
      tmp1 <- split_area_level(tmp1, n_plots = n_plots)
    }

    # plot
    plots <- lapply(seq_along(tmp), function(i) {
        lapply(seq_along(tmp[[i]]), function(j) {

            dat <- tmp[[i]][[j]]
            dat1 <- tmp1[[i]][[j]]

            plot_title <- paste(
                spec_title,
                dat$iso3[1],
                # dat$area_level[1],
                # dat$area_level_label[1],
                sep = ", "
            )

            if (province_split) {

                if (!is.na(dat$parent_area_name[1])) {
                  plot_title <- paste(
                      plot_title,
                      paste0("Parent Area: ", dat$parent_area_name[1]),
                      sep = ", "
                  )
                }
            } else {
                plot_title <- paste(
                    plot_title,
                    dat$area_level[1],
                    dat$area_level_label[1],
                    sep = ", "
                )
            }

            ggplot(dat,
                   aes(x = year,
                       group = type,
                       fill = type)) +
                # Adding target line to prevalence
                geom_hline(yintercept = hor_line,
                           size = 1,
                           linetype = "dashed",
                           colour = "grey50") +
                # Prevalence as area plot
                geom_area(aes(y = 100 * mean.y)) +
                suppressWarnings(geom_text(data = dat1,
                                           aes(x = 2008,
                                               y = 100 * mean.y,
                                               fill = NA,
                                               label = if_else(year %in% c(2008),
                                                               scales::percent(mean.y, 1),
                                                               NA_character_)),
                                           fontface = "bold",
                                           vjust = 0,
                                           colour = "grey30",
                                           size = 6,
                                           nudge_y = 0.02,
                                           show.legend = FALSE)) +
                suppressWarnings(geom_text(data = dat1,
                                           aes(x = 2020,
                                               y = 100 * mean.y,
                                               fill = NA,
                                               label = if_else(year %in% c(2020),
                                                               scales::percent(mean.y, 1),
                                                               NA_character_)),
                                           fontface = "bold",
                                           vjust = 0,
                                           colour = "grey30",
                                           size = 6,
                                           nudge_y = 0.02,
                                           show.legend = FALSE)) +
                # Setting for the axes
                scale_x_continuous(breaks = seq(2008, 2020, by = 2),
                                   limits = c(2007.25, 2020.75)) +
                scale_y_continuous(breaks = scales::pretty_breaks(5),
                                   limits = c(0, 108)) +
                # Setting colour palette
                scale_fill_manual(
                    values = wesanderson::wes_palette("Zissou1", 3)[c(1, 3)]
                ) +
                # Plotting labels
                labs(x = "",
                     y = "Circumcision Coverage (%)",
                     fill = "") +
                facet_wrap(. ~ area_name) +
                ggtitle(plot_title) +
                # Minimal theme
                theme_minimal() +
                # Altering plot text size
                theme(
                    plot.title = element_text(size = 26, hjust = 0.5),
                    axis.text.y = element_blank(),
                    strip.text = element_text(size = 20, face = "bold"),
                    strip.background = element_blank(),
                    axis.title = element_text(size = 16),
                    # panel.grid.minor = element_blank(),
                    legend.text = element_text(size = 12),
                    axis.text.x = element_text(size = 12,
                                               angle = 45,
                                               hjust = 1,
                                               vjust = 1),
                    legend.position = "bottom",
                    plot.tag = element_text(face = "bold", size = 22)
                )
        })
    })

    # save, if desired, else return plots
    if (!is.null(str_save)) {
        ggsave(
            filename = str_save,
            plot = gridExtra:: marrangeGrob(rlang::squash(plots),
                                            nrow = 1,
                                            ncol = 1),
            dpi = "retina",
            width = save_width,
            height = save_height
        )
    } else {
        return(rlang::squash(plots))
    }
}

# fig 5 (for one type, plot age vs coverage %, for multiple years and
# including error bounds)
plt_age_coverage_multi_years <- function(
    results_age,
    areas,
    spec_types = "MC coverage",
    spec_years, # = c(2009, 2015, 2021)
    area_levels = unique(results_age$area_level),
    spec_model = "No program data",
    spec_ages = c(0, 60),
    province_split = FALSE,
    str_save = NULL,
    spec_title = paste0("Male Circumcision Coverage vs Age"),
    save_width = 9,
    save_height = 7,
    n_plots = 12
) {

    # Subsetting
    tmp <- results_age %>%
        filter(
            type      %in% spec_types,
            year     %in% spec_years,
            area_level %in% area_levels,
            model == spec_model
        ) %>%
        # multiply prevalence by 100
        mutate(across(c(mean, lower, upper), ~ . * 100))

    # add in required columns from areas and order area names (make function)
    tmp <- add_area_info(tmp, areas)

    # split by area level and number of desired plots
    if (province_split) {
        # if desired, split by parent area id instead of by n_plots
        tmp <- dplyr::group_split(tmp, area_level) %>%
            purrr::map(~ dplyr::group_split(.x, parent_area_id))
    } else {
        tmp <- split_area_level(tmp, n_plots = n_plots)
    }

    plots <- lapply(tmp, function(i) {
        lapply(i, function(j) {

            # title displaying area level and label
            spec_title <- paste(
                spec_title,
                j$iso3[1],
                sep = ", "
            )
            if (province_split) {
                if (is.na(j$parent_area_name[1])) {
                    parent_lab <- NULL
                } else {
                    parent_lab <- paste0("Parent Area: ", j$parent_area_name[1])
                }
                spec_title <- paste(
                    spec_title,
                    parent_lab,
                    sep = ", "
                )
            } else {
                spec_title <- paste(
                    spec_title,
                    j$area_level[1],
                    j$area_level_label[1],
                    sep = ", "
                )
            }

            p <- ggplot(j,
                   aes(x = age,
                       group = as.factor(year),
                       fill = as.factor(year),
                       colour = as.factor(year))) +
                # Adding target line to prevalence
                geom_hline(yintercept = 90,
                           size = 1,
                           linetype = "dashed",
                           colour = "grey50") +
                # Prevalence as area plot
                geom_ribbon(aes(ymin = lower,
                                ymax = upper),
                            colour = NA,
                            alpha = 0.5) +
                # Prevalence as area plot
                geom_line(aes(y = mean),
                          size = 1) +
                # Setting for the axes
                scale_x_continuous(
                    breaks = seq(spec_ages[1], spec_ages[2], by = 10)
                ) +
                scale_y_continuous(
                    breaks = scales::pretty_breaks(5),
                    limits = c(0, 100)) +
                # Setting colour palette
                scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)) +
                scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)) +
                # Plotting labels
                ggtitle(spec_title) +
                labs(x = "Age",
                     y = "Circumcision coverage (%)",
                     colour = "",
                     fill = "") +
                # Minimal theme
                theme_minimal() +
                # Geofacet
                # facet_geo(~ area_id # ,
                #          grid = zaf_district_grid,
                #          label = "name_district"
                # ) +
                # Altering plot text size
                theme(axis.text = element_text(size = 16),
                      strip.text = element_text(size = 16),
                      strip.background = element_blank(),
                      # panel.grid = element_blank(),
                      # panel.grid.minor = element_blank(),
                      axis.title = element_text(size = 20),
                      # plot.title = element_text(size = 40, hjust = 0.5),
                      plot.title = element_text(size = 24, hjust = 0.5),
                      legend.text = element_text(size = 20),
                      legend.position = "bottom")

            if (length(spec_types) > 1) {
                p <- p + facet_grid(type ~ area_name)
            } else p <- p + facet_wrap(. ~ area_name) +

            return(p)
        })
    })
    if (!is.null(str_save)) {
        ggsave(
            filename = str_save,
            plot = gridExtra:: marrangeGrob(rlang::squash(plots), nrow = 1, ncol = 1),
            dpi = "retina",
            width = save_width,
            height = save_height
        )
    } else {
        return(rlang::squash(plots))
    }
}

# fig 6, plotting distributions/ridges for mean circ age for TMIC and MMC-nT
# for different areas
plt_circ_age_ridge <- function(
    results_age,
    areas,
    spec_years = last(results_age$year),
    area_levels = unique(results_age$area_level),
    spec_model = "No program data",
    spec_ages = 0:30,
    str_save = NULL,
    save_width = 9,
    save_height = 7,
    n_plots = 8
) {

    # temp fix
    if (length(spec_ages) != 31 || !all(spec_ages == 0:30)) {
      spec_ages <- 0:30
    }

    # Keeping relevant information
    tmp <- results_age %>%
        # Only keeping relevant data
        filter(
            type %in% c("MMC-nTs performed", "TMICs performed") ,
            area_level %in% area_levels,
            model == spec_model,
            year %in% spec_years,
            age %in% spec_ages
            # area_level %in% c(0:1)# ,
            # model == "With program data"
        ) %>%
    # Getting density for the ridge plot
        # Grouping for normalising
        group_by(area_id, year, type) %>%
        # Estimating density
        mutate(density = mean / (2 * sum(mean))) %>%
        ungroup() %>%
        # Altering labels for the plot
        mutate(type = ifelse(grepl("MMC-nT", type), "Medical", "Traditional")) #  %>%
        # order_area_name()

    tmp2 <- tmp %>%
        group_by(area_id, area_name, type, year) %>%
        summarise(
            average_age = weighted.mean(age, w = density),
            average_age_lower = weighted.mean(age, w = lower),
            average_age_upper = weighted.mean(age, w = upper),
            .groups = "drop"
        )

    # add in required columns from areas and order area names
    tmp <- add_area_info(tmp, areas)
    tmp2 <- add_area_info(tmp2, areas)

    # split by area level and number of desired plots
    # tmp <- split_area_level(tmp, n_plots = n_plots, years = TRUE)
    # tmp2 <- split_area_level(tmp2, n_plots = n_plots, years = TRUE)
    tmp <- list(tmp)
    tmp2 <- list(tmp2)

    plots <- lapply(seq_along(tmp), function(i) {
        # lapply(seq_along(tmp[[i]]), function(j) {
          # lapply(seq_along(tmp[[i]][[j]]), function(k) {

            # plt_data <- tmp[[i]][[j]][[k]]
            # plt_data2 <- tmp2[[i]][[j]][[k]]
            plt_data <- tmp[[i]]
            plt_data2 <- tmp2[[i]]

            spec_title <- paste(
              plt_data$year[1],
              plt_data$iso3[1],
              plt_data$area_level[1],
              plt_data$area_level_label[1],
              sep = ", "
            )

            ggplot(plt_data,
                   aes(x = age,
                       y = area_name,
                       height = density,
                       fill = type,
                       colour = type)) +
              ggridges::geom_density_ridges(
                stat = "identity",
                scale = 1,
                alpha = 0.7,
                colour = NA
              )  +
              # Adding average age of circumcision
              geom_point(data = plt_data2,
                         aes(x = average_age,
                             y = as.integer(area_name) - 0.05,
                             colour = type),
                         inherit.aes = FALSE,
                         show.legend = FALSE) +
              # Adding uncertainty interval of average age of circumcision
              geom_segment(data = plt_data2,
                           aes(x = average_age_lower,
                               xend = average_age_upper,
                               y = as.integer(area_name) - 0.05,
                               yend = as.integer(area_name) - 0.05,
                               colour = type),
                           inherit.aes = FALSE,
                           show.legend = FALSE) +
              # Colour palette
              scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1, 3)]) +
              scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1, 3)]) +
              # Setting theme
              theme_minimal() +
              # Splitting by circumcision type
              # facet_grid(. ~ type) +
              # Setting labels
              ggtitle(spec_title) +
              labs(y = NULL,
                   x = "Age at circumcision",
                   colour = NULL,
                   fill = NULL) +
              # Changing plot themes
              theme(
                axis.title = element_text(size = 24),
                axis.text = element_text(size = 20, face = "bold"),
                # axis.text.x = element_text(size = 20, face = "bold"),
                # axis.text.y = element_text(size = 16),
                plot.title = element_text(size = 30, hjust = 0.5),
                strip.text = element_text(size = 16),
                strip.background = element_blank(),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 24),
                legend.position = "bottom",
                panel.spacing = unit(0.2, "lines")
              )
          # })
        # })
    })

    plots <- rlang::squash(plots)
    if (!is.null(str_save)) {
        ggsave(
            filename = str_save,
            plot = gridExtra:: marrangeGrob(plots, nrow = 1, ncol = 1),
            dpi = "retina",
            width = save_width,
            height = save_height
        )
    } else {
        return(plots)
    }
}

#### Plots for comparing survey points with model fit ####
plt_MC_modelfit_spec_age <- function(
  df_results, df_results_survey,
  mc_type_model, mc_type_survey,
  age_per, years, area_level_select = unique(df_results$area_level),
  model_type = "No program data",
  facet_vars = "area_name",
  col_fill_vars = "parent_area_name",
  province_split = FALSE,
  xlab, ylab, title, str_save = NULL,
  save_width = 16, save_height = 12, n_plots = 12) {

  # filter data accordingly
  initial_filter <- function(.data, type_filter) {
    .data <- .data %>%
      filter(
        year %in% years,
        area_level %in% area_level_select,
        age_group %in% age_per,
        type == type_filter
      )
    if ("model" %in% names(.data)) .data <- .data[.data$model == model_type, ]
    return(.data)
  }
  df_results <- initial_filter(df_results, mc_type_model)
  df_results_survey <- initial_filter(df_results_survey, mc_type_survey)

  # make sure areas are the same for both
  df_results_survey <- df_results_survey %>%
      filter(
        area_name %in% df_results$area_name,
        year %in% df_results$year
      )

  # do the same for model estimates if they don't align with surveys
  if (
    length(unique(df_results$area_id)) !=
    length(unique(df_results_survey$area_id))
  ) {
    message("Area IDs in model estimates and surveys are misaligned")
    df_results <- df_results %>%
      filter(
        area_name %in% df_results_survey$area_name # ,
        # year %in% df_results_survey$year
      )
  }

  # manipulate facet variables
  # by_expr <- rlang::enexpr(facet_vars)
  # if(length(by_expr) == 1) {
  #   args_vars <- as.list(by_expr)
  # } else {
  #   args_vars <- as.list(by_expr)[-1]
  # }
  # quoted_args_vars <- sapply(args_vars, rlang::quo_name)

  # by <- rlang::enexpr(facet_vars) %>%
  #   tidyselect::eval_select(df_results)

  # split results by area level, and number of plots desired
  # df_results <- split_area_level(df_results, n_plots = n_plots)
  # df_results_survey <- split_area_level(df_results_survey, n_plots = n_plots)

  if (province_split) {
      # if desired, split by parent area id instead of by n_plots
      df_results <- dplyr::group_split(df_results, area_level) %>%
          purrr::map(~ dplyr::group_split(.x, parent_area_id))
      df_results_survey <- dplyr::group_split(df_results_survey, area_level) %>%
          purrr::map(~ dplyr::group_split(.x, parent_area_id))
  } else {

    # split by area level and number of desired plots
      df_results <- split_area_level(df_results, n_plots = n_plots)
      df_results_survey <- split_area_level(df_results_survey, n_plots = n_plots)
  }


  # plot for each (nested) loop
  plots <- lapply(seq_along(df_results), function(i) {
    lapply(seq_along(df_results[[i]]), function(j) {

      plt_data1 <- df_results[[i]][[j]]
      plt_data2 <- df_results_survey[[i]][[j]]

      if (i == 1 && j == 1) {
          plt_data1$parent_area_name <- "NA"
      }

      # get specific title for each plot page
      add_title <- paste(
        plt_data1$iso3[1],
        plt_data1$area_level[1],
        plt_data1$area_level_label[1],
        sep = ", "
      )
      
      if (province_split == TRUE && plt_data1$parent_area_name != "NA") {
        add_title <- paste0(
          add_title, 
          ", Parent Area: ",
          plt_data1$parent_area_name[1])
      }

      ggplot(plt_data1, aes(x = year)) +
      # ggplot(df_results[[i]], aes(x = year)) +
        # Credible interval
        # geom_ribbon(aes(ymin = lower, ymax = upper, fill = parent_area_name),
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = age_group),
        # geom_ribbon(aes(ymin = lower, ymax = upper, fill = col_fill_vars),
                    # colour  = NA,
                    alpha = 0.5) +
        # Modelled rate
        # geom_line(aes(y = mean, col = parent_area_name), size = 1) +
        geom_line(aes(y = mean, col = age_group), size = 1) +
        # geom_line(aes(y = mean, col = col_fill_vars), size = 1) +
        geom_pointrange(
            data = plt_data2,
            aes(
              y    = mean,
              ymin = lower,
              ymax = upper,
              col  = age_group
              # col = col_fill_vars
            ),
            # colour = "black",
            show.legend = FALSE
        ) +
        # Labels
        labs(x = xlab, y = "Circumcision prevalence", colour = "", fill = "") +
        ggtitle(paste0(title, add_title)) +
        # Faceting by area
        facet_wrap(~ area_name) +
        # facet_wrap(quoted_args_vars) +
        # facet_wrap(names(by)) +
        scale_x_continuous(breaks = seq(min(plt_data1$year),
        # scale_x_continuous(breaks = seq(min(df_results[[i]]$year),
                                        2021,
                                        by = 2)) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                           limits = c(0, 1),
                           label = scales::label_percent(accuracy = 1)) +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) +
        theme(axis.text = element_text(size = 14),
              strip.text = element_text(size = 14),
              axis.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              axis.text.x = element_text(angle = -90, vjust = 0.5, size = 14),
              plot.title = element_text(size = 18, hjust = 0.5),
              legend.position = "bottom")
    })
  })

  # flatten nested list of plots
  plots <- rlang::squash(plots)
  if (!is.null(str_save)) {
    # save
    plots <-  gridExtra::marrangeGrob(plots, nrow = 1, ncol = 1)
    ggsave(filename = str_save,
           plot = plots,
           dpi = "retina",
           width = save_width,
           height = save_height)
  } else {
   return(plots)
  }
}

plt_MC_modelfit <- function(df_results, df_results_survey, mc_type_model,
                            mc_type_survey,
                            age_per = c(
                              "0-4",   "5-9",   "10-14", "15-19", "20-24",
                              "25-29", "30-34", "35-39", "40-44",
                              "45-49", "50-54", "54-59", "60-64"
                            ),
                            survey_years,
                            model_type,
                            area_level_select = unique(df_results$area_level),
                            province_split = FALSE,
                            # area_level_select,
                            # facet_vars = "area_name",
                            # col_fill_vars = "parent_area_name",
                            # for year: "facet", have separate plots ("split"), or "colour"
                            facet_year = "split",
                            xlab, ylab, title,
                            str_save = NULL,
                            save_width = 16,
                            save_height = 12,
                            n_plots = 12) {

  # Preparing dataset for plots
  initial_filter <- function(.data, spec_type) {
    .data <- .data %>%
      filter(
        type == spec_type,
        age_group %in% age_per,
        year %in% survey_years,
        area_level %in% area_level_select
      )
    if ("model" %in% names(.data)) .data <- .data[.data$model == model_type, ]
    # if (nrow(.data) == 0) stop("Filtering has resulted in an empty dataframe")
    return(.data)
  }
  # browser()
  tmp1 <- initial_filter(df_results, mc_type_model)
  tmp2 <- initial_filter(df_results_survey, mc_type_survey)

  # make sure surveys and model estimates are aligned
  tmp2 <- tmp2 %>%
    filter(
      area_name %in% tmp1$area_name,
      year %in% tmp1$year
    )

  if (
    length(unique(tmp1$area_id)) !=
    length(unique(tmp2$area_id))
  ) {
    message("Area IDs in model estimates and surveys are misaligned")
    tmp1 <- tmp1 %>%
      filter(
        area_name %in% tmp2$area_name,
        year %in% tmp2$year
      )
  }


  # Ordering age groups (needed??)
  tmp1$age_group <- as.numeric(factor(tmp1$age_group, levels = age_per))
  tmp2$age_group <- as.numeric(factor(tmp2$age_group, levels = age_per))

  # make sure areas are the same for both
  tmp2 <- filter(tmp2, area_name %in% tmp1$area_name)
  tmp1 <- filter(tmp1, year %in% tmp2$year) # make sure years are the same

  # replace NAs with "NA" in parent_area_name
  tmp1 <- tmp1 %>%
    mutate(
      parent_area_name = ifelse(
        is.na(parent_area_name),
        "NA",
        parent_area_name
      )
   )

  # final label for plot
  final_label <- last(age_per)
  final_label <- as.numeric(stringr::str_split(final_label, "-")[[1]][[2]])
  final_label <- paste0(final_label + 1, "+")

  # manipulate facet variables
  # by_expr <- rlang::enexpr(facet_vars)
  # if(length(by_expr) == 1) {
  #   args_vars <- as.list(by_expr)
  # } else {
  #   args_vars <- as.list(by_expr)[-1]
  # }
  # quoted_args_vars <- sapply(args_vars, rlang::quo_name)

  # by <- rlang::enexpr(facet_vars) %>%
  #   tidyselect::eval_select(tmp1)

  # split results by area level, year and number of plots desired
  # if ("year" %in% col_fill_vars) {
  #   year_split <- FALSE
  # } else year_split <- TRUE
  if (facet_year == "split") {
    year_split <- TRUE
  } else {
    year_split <- FALSE
  }

  if (province_split) {
      # if desired, split by parent area id instead of by n_plots
      tmp1 <- dplyr::group_split(tmp1, area_level) %>%
          purrr::map(~ dplyr::group_split(.x, parent_area_id))
      tmp2 <- dplyr::group_split(tmp2, area_level) %>%
          purrr::map(~ dplyr::group_split(.x, parent_area_id))
  } else {
    # split by area level and number of desired plots (and year, if desired)
    tmp1 <- split_area_level(tmp1, year = year_split, n_plots = n_plots)
    tmp2 <- split_area_level(tmp2, year = year_split, n_plots = n_plots)
  }


  plot_fun <- function(plt_data1, plt_data2) {
    # get specific title for each plot page
    add_title <- paste(
      plt_data1$iso3[1],
      plt_data1$area_level[1],
      plt_data1$area_level_label[1],
      sep = ", "
    )
  
    if (province_split == TRUE && plt_data1$parent_area_name[1] != "NA") {
      add_title <- paste0(
        add_title, 
        ", Parent Area: ",
        plt_data1$parent_area_name[1])
    }

    if (facet_year == "colour") {
      colour_var <- "year"
    } else {
      colour_var <- "parent_area_id"
    }

    if ("parent_area_id" %in% names(plt_data1)) {
      plt_data1 <- plt_data1 %>%
        mutate(
          # parent_area_name = ifelse(
          #   is.na(parent_area_name),
          #   "NA",
          #   parent_area_name
          # ),
          parent_area_id = ifelse(
              is.na(parent_area_id),
              "NA",
              parent_area_id
          )
        )
    }
    if ("parent_area_id" %in% names(plt_data2)) {
      plt_data2 <- plt_data2 %>%
          mutate(
              # parent_area_name = ifelse(
              #     is.na(parent_area_name),
              #     "NA",
              #     parent_area_name
              # ),
              parent_area_id = ifelse(
                  is.na(parent_area_id),
                  "NA",
                  parent_area_id
              )
          )
    }



    p <- ggplot(plt_data1, aes(x = age_group)) +
      # geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(year)),
      geom_ribbon(
        aes(ymin = lower, ymax = upper, fill = as.factor(.data[[colour_var]])),
        alpha = 0.75
      ) +
      # geom_line(aes(y = mean, col = as.factor(year)),
      geom_line(
        aes(y = mean, col = as.factor(.data[[colour_var]])),
        size = 1
      ) +
      geom_pointrange(
        data = plt_data2,
        aes(
          y      = mean,
          ymin   = lower,
          ymax   = upper,
          # colour = as.factor(year)
          colour = as.factor(.data[[colour_var]])
        ),
        show.legend = FALSE
      ) +
      # Labels
      labs(x =  xlab, y = ylab, colour = "", fill = "") +
      ggtitle(paste0(title, add_title)) +
      scale_x_continuous(
        breaks = 1:(length(age_per) + 1),
        labels = c(age_per, final_label),
      ) +
      scale_y_continuous(breaks = seq(0, 1,  by =  0.2),
                         limits = c(0, 1),
                         labels = scales::label_percent()) +
      theme_bw() +
      theme(axis.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 18),
            axis.text.x = element_text(angle = -90, vjust = 0.5, size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "bottom")

    if (facet_year == "facet") {
      # p <- p + facet_wrap(
      #   ~area_name + year,
      #   nrow = length(survey_years)
      # )
      p <- p + facet_grid(year ~ area_name)
    } else {
      p <- p + facet_wrap(~area_name)
      # facet_wrap(quoted_args_vars)
      # facet_wrap(names(by))
    }
    return(p)
  }

  recursive_plot_fun <- function(plt_data1, plt_data2, ...) {
    if (!inherits(plt_data1, "data.frame")) {
      lapply(seq_along(plt_data1), function(i) {
        recursive_plot_fun(plt_data1[[i]], plt_data2[[i]], ...)
      })
    } else {
      plot_fun(plt_data1, plt_data2, ...)
    }
  }

  # plot for each (nested) loop
  # plots <- recursive_plot_fun(tmp1, tmp2)
  plots <- lapply(seq_along(tmp1), function(i) {
    recursive_plot_fun(tmp1[[i]], tmp2[[i]])
  })

  # flatten nested list of plots
  plots <- rlang::squash(plots)
  # if (!"data.frame" %in% class(plots[[1]][[1]])) {
  #   plots <- purrr::flatten(plots)
  # }

  if (!is.null(str_save)) {
    # save
    plots <-  gridExtra::marrangeGrob(plots, nrow = 1, ncol = 1)
    ggsave(filename = str_save,
           plot = plots,
           dpi = "retina",
           width = save_width,
           height = save_height)
  } else {
    return(plots)
  }
}

#### DMPPT2 Comparison Plots ####

# would make a good function to add to aggregations code! Doesn't work for 0+ though
change_agegroup_convention <- function(.data) {

  lower <- as.numeric(substr(.data$age_group, 3, 4))
  if (all(!is.na(as.numeric(lower)))) {
    upper <- as.numeric(substr(.data$age_group, 7, 8))
    .data$age_group <- paste(lower, upper, sep = "-")
  }
  return(.data)
}

# filtering function (shared by both functions below)
initial_filter <- function(.data, col, val) {
  if (col %in% names(.data) & !any(is.null(val))) {
    .data <- filter(.data, !!rlang::sym(col) %in% val)
  }
  return(.data)
}

# recursively apply filtering function (removing need to loop)
initial_filter_recursive <- function(.data, cols, vals) {

  .data <- initial_filter(.data, cols[1], vals[[1]])
  if (length(cols) == 1) {
    return(.data)
  } else {
    initial_filter_recursive(.data, cols[-1], vals[-1])
  }
}


# shamelessly stolen from naomi.utils@sexbehav-vars-jeff, used to calculate
# credible intervals on the logit scale
calc_logit_confint <- function(estimate, std_error, tail, conf.level = 0.95) {

  stopifnot(length(estimate) == length(std_error))
  stopifnot(tail %in% c("lower", "upper"))
  stopifnot(conf.level > 0 & conf.level < 1)

  crit <- qnorm(1 - (1 - conf.level) / 2) * switch(tail, "lower" = -1, "upper" = 1)
  lest <- stats::qlogis(estimate)
  lest_se <- std_error / (estimate * (1 - estimate))

  ifelse(estimate < 1 & estimate > 0, stats::plogis(lest + crit * lest_se), NA_real_)
}

# function to produce scatter plots of coverage vs year for one age_group
plt_dmppt2_compare_year <- function(circ_data,
                                    dmppt2_data,
                                    survey_data,
                                    age_per = "15-49",
                                    area_levels = NULL,
                                    years = NULL,
                                    model_type = NULL,
                                    xlab = "Year",
                                    ylab = "Circumcision Coverage",
                                    title,
                                    str_save = NULL,
                                    save_width = NULL,
                                    save_height = NULL,
                                    n_plots = 12) {

  # circ_data_test <- circ_data
  # dmppt2_data_test <- dmppt2_data
  # survey_data_test <- survey_data
  # circ_data <- circ_data_test
  # dmppt2_data <- dmppt2_data_test
  # survey_data <- survey_data_test
  # area_levels <- model_type <- NULL
  # plt_start_year <- min(circ_data$year)
  # age_per = "15-49"
  # years = plt_start_year:2021
  # model_type = NULL
  # n_plots <- 12
  # xlab = "Age Group"
  # ylab = "Circumcision Prevalence"
  # # title = "Test"
  # title <- paste0(
  #     "15-29 Prevalence - ",
  #     "Black line denotes DMPPT2 coverage, Blue dots denote Surveyed coverage - "
  # )

  # initial filtering
  cols <- c("age_group", "area_level", "year", "model")
  vals <- list(age_per, area_levels, years, model_type)
  for (i in seq_along(cols)) {
    circ_data <- initial_filter(circ_data, cols[i], vals[[i]])
    dmppt2_data <- initial_filter(dmppt2_data, cols[i], vals[[i]])
    survey_data <- initial_filter(survey_data, cols[i], vals[[i]])
  }

  # join data together
  circ_data <- circ_data %>%
    left_join(
      select(dmppt2_data,
             area_id,
             year,
             age_group,
             dmppt2_circumcision_coverage = mean)
    ) %>%
    left_join(
      select(
        survey_data,
        area_id,
        year,
        age_group,
        p_ind = mean,
        sd_ind = sd
      )
    )

  # For LSO, DMPPT2 models medical, not all circumcision
  if (all(circ_data$iso3 == "LSO")) {
    circ_data <- circ_data %>%
      filter(type == "MMC coverage")
  } else {
    circ_data <- circ_data %>%
      filter(type == "MC coverage")
  }

  # replace NAs with "NA" in parent_area_name
  circ_data <- circ_data %>%
    mutate(
      parent_area_name = ifelse(
        is.na(parent_area_name),
        "NA",
        parent_area_name
      )
    )

  # split results by area level, and number of plots desired
  circ_data <- split_area_level(circ_data, n_plots = n_plots)

  # plot for each (nested) loop
  plots <- lapply(seq_along(circ_data), function(i) {
    lapply(seq_along(circ_data[[i]]), function(j) {

      # browser()
      spec_circ_data <- circ_data[[i]][[j]]

      # get specific title for each plot page
      add_title <- paste(
        spec_circ_data$iso3[1],
        spec_circ_data$area_level[1],
        spec_circ_data$area_level_label[1],
        sep = ", "
      )

      p <- ggplot(spec_circ_data, aes(x = year)) +
        # Credible interval
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = parent_area_name),
                    # colour  = NA,
                    alpha = 0.5) +
        # Modelled rate
        geom_line(aes(y = mean, col = parent_area_name),
                  show.legend = FALSE,
                  size = 1) +
        geom_line(
          aes(y = dmppt2_circumcision_coverage, col = "black"),
          colour = "black"
        ) # +
      # geom_point(# data = survey_data[[i]][[j]],
      #     # geom_point(data = df_results_survey[[i]],
      #     aes(y = p_ind, col = "blue"),
      #     colour = "blue"
      # ) +

      if (!all(is.na(spec_circ_data$p_ind))) {
        p <- p +
          geom_pointrange(
            aes(
              y = p_ind,
              # ymin = p_ind - sd_ind,
              ymin = calc_logit_confint(p_ind, sd_ind, "lower"),
              # ymax = p_ind + sd_ind
              ymax = calc_logit_confint(p_ind, sd_ind, "upper")
            ),
            colour = "blue"
          )
      }

      p <- p +
        # Labels
        labs(x = xlab, y = ylab, colour = "", fill = "Parent Area") +
        ggtitle(paste0(title, add_title)) +
        # Faceting by area
        facet_wrap(~area_name) +
        scale_x_continuous(breaks = seq(min(circ_data[[i]][[j]]$year),
                                        2021,
                                        by = 2)) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                           limits = c(0, 1),
                           label = scales::label_percent(accuracy = 1)) +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2))

      # don't add parent area legend for i = j = 1 (country level)
      if (i == 1 & j == 1) {
        p <- p +
          theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
                legend.position = "none"
          )
      } else {
        p <- p +
          theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
                legend.position = "bottom"
          )
      }
      return(p)
    })
  })

  plots <- rlang::squash(plots)
  if (!is.null(str_save)) {
    # save
    plots <-  gridExtra::marrangeGrob(plots, nrow = 1, ncol = 1)
    ggsave(filename = str_save,
           plot = plots,
           dpi = "retina",
           width = save_width,
           height = save_height)
  } else {
    return(plots)
  }
}

# function to produce scatter plots of coverage vs age-group for one year
plt_dmppt2_compare_age_group <- function(circ_data,
                                         dmppt2_data,
                                         survey_data,
                                         age_per, # required arg
                                         area_levels = NULL,
                                         years = NULL,
                                         model_type = NULL,
                                         xlab = "Year",
                                         ylab = "Circumcision Prevalence",
                                         title,
                                         str_save = NULL,
                                         save_width = NULL,
                                         save_height = NULL,
                                         n_plots = 1) {


  # circ_data_test <- circ_data
  # dmppt2_data_test <-dmppt2_data
  # survey_data_test <- survey_data
  #
  # circ_data <- circ_data_test
  # dmppt2_data <- dmppt2_data_test
  # survey_data <- survey_data_test
  # age_per = c("0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", "30-34",
  #             "35-39", "40-44", "45-49", "50-54", "54-59", "60-64")
  # years =  unique(c(
  #     max(dmppt2_data$year),
  #     max(survey_data$year),
  #     max(2013, min(dmppt2_data$year))
  # ))
  #
  # xlab = "Age Group"
  # ylab = "Circumision Prevalence"
  # title = main_title
  # title <- "test"
  # str_save = "test4.pdf"
  # save_width = 16
  # save_height = 12
  # n_plots = 6


  # ensure age_per is ordered properly
  age_per <- age_per[
    order(as.numeric(sapply(stringr::str_split(age_per, "-"), `[[`, 2)))
  ]

  # take age above age_per as last label for plot
  final_label <- last(age_per)
  final_label <- as.numeric(stringr::str_split(final_label, "-")[[1]][[2]])
  final_label <- paste0(final_label + 1, "+")

  cols <- c("age_group", "area_level", "year", "model")
  vals <- list(age_per, area_levels, years, model_type)
  for (i in seq_along(cols)) {
    circ_data <- initial_filter(circ_data, cols[i], vals[[i]])
    dmppt2_data <- initial_filter(dmppt2_data, cols[i], vals[[i]])
    if (!is.null(survey_data)) survey_data <- initial_filter(survey_data,
                                                             cols[i],
                                                             vals[[i]])
  }

  # For LSO, DMPPT2 models medical, not all circumcision
  if (all(circ_data$iso3 == "LSO")) {
    circ_data <- circ_data %>%
      filter(type == "MMC coverage")
  } else {
    circ_data <- circ_data %>%
      filter(type == "MC coverage")
  }

  # join data
  circ_data <- circ_data %>%
    left_join((dmppt2_data %>%
                 select(area_id, year, age_group,
                        dmppt2_circumcision_coverage)))
  if (!is.null(survey_data)) {
    circ_data <- circ_data %>%
      left_join((survey_data %>%
                   select(
                     area_id, year, age_group, p_ind = mean, sd_ind = sd
                   )))
  } else circ_data$p_ind <- circ_data$sd_ind <- NA
  circ_data <- circ_data %>%
    tidyr::pivot_longer(c(mean, dmppt2_circumcision_coverage, p_ind),
                        names_to = "source") %>%
    mutate(
      # only have error bounds for threemc fit and survey points
      across(sd:upper, ~ifelse(source == "mean", ., NA)),
      sd_ind = ifelse(source == "p_ind", sd_ind, NA),
      # impute sd with sd_ind for p_ind
      sd = ifelse(!is.na(sd_ind), sd_ind, sd),
      # recompute upper and lower error bounds
      lower = calc_logit_confint(value, sd, "lower"),
      upper = calc_logit_confint(value, sd, "upper"),
      # change labels
      source = ifelse(source == "mean", "threemc",
                      ifelse(source == "p_ind", "Survey", "DMPPT2")),
      # order age groups and data sources
      age_group = as.numeric(factor(age_group, levels = age_per)),
      source = factor(source,
                      levels = c("threemc", "DMPPT2", "Survey"))
    ) %>%
    filter(!is.na(value))

  # replace NAs with "NA" in parent_area_name
  circ_data <- circ_data %>%
    mutate(
      parent_area_name = ifelse(
        is.na(parent_area_name),
        "NA",
        parent_area_name
      )
  )

  # split results by area level, year and number of plots desired
  circ_data <- split_area_level(circ_data, year = TRUE, n_plots = n_plots)

  # if (n_plots > 1) {
  #  circ_data <-
  # }

  # remove one list level from circ_data
  # circ_data <- purrr::flatten(circ_data)
  # circ_data <- lapply(circ_data, purrr::flatten)

  # plot for each (nested) loop
  plots <- lapply(seq_along(circ_data), function(i) { # area
    lapply(seq_along(circ_data[[i]]), function(j) { # year
      lapply(seq_along(circ_data[[i]][[j]]), function(k) { # cap n plots

        spec_circ_data <- circ_data[[i]][[j]][[k]]
        # spec_circ_data <- circ_data[[i]][[j]]

        # get specific title for each plot page
        add_title <- paste(
          spec_circ_data$iso3[1],
          paste0("area_level: ", spec_circ_data$area_level[1]),
          spec_circ_data$area_level_label[1],
          spec_circ_data$year[1],
          sep = ", "
        )

        # plot barchart for different ages and sources for each area
        ggplot(spec_circ_data,
               aes(x = age_group, y = value, fill = source)) +
          geom_bar(stat = "identity", position = position_dodge()) +
          geom_errorbar(
            aes(x = age_group, ymin = lower, ymax = upper),
            width = 0.5,
            position = position_dodge(width = 0.9)
          ) +
          scale_fill_brewer(palette = "Dark2") +
          scale_x_continuous(
            # breaks = 1:14,
            # labels = c("0-4","5-9","10-14","14-19","20-24","25-29",
            #            "30-34","35-39","40-44","45-49","50-54",
            #            "55-59","60-64","65+")
            breaks = 1:(length(age_per) + 1),
            labels = c(age_per, final_label),
          ) +
          scale_y_continuous(breaks = seq(0, 1,  by =  0.2),
                             limits = c(0, 1),
                             labels = scales::label_percent()) +
          theme_bw() +
          labs(x =  xlab, y = ylab, colour = "", fill = "") +
          ggtitle(paste0(title, add_title)) +
          theme(
            axis.text = element_text(size = 14),
            strip.text = element_text(size = 12),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 18),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5,
                                       size = 10),
            legend.position = "bottom") +
          facet_wrap(~ area_name)
          # facet_wrap(~ area_name + year)

        # ggplot(spec_circ_data, aes(x = age_group)) +
        #     geom_ribbon(aes(ymin = lower, ymax = upper, fill = parent_area_name),
        #                 alpha = 0.75,
        #                 # colour = NA,
        #                 # fill = "darkgrey"
        #     ) +
        #     geom_line(aes(y = mean, col = parent_area_name),
        #               size = 1
        #               # colour = "black"
        #     ) +
        #     geom_point(data = spec_dmppt2_data,
        #                aes(y = dmppt2_circumcision_coverage),
        #                colour = "black",
        #                show.legend = FALSE) +
        #     # Labels
        #     labs(x =  xlab, y = ylab, colour = "", fill = "") +
        #     ggtitle(paste0(title, add_title)) +
        #     scale_x_continuous(breaks = 1:14,
        #                        labels = c("0-4","5-9","10-14","14-19","20-24","25-29",
        #                                   "30-34","35-39","40-44","45-49","50-54",
        #                                   "55-59","60-64","65+")) + # should change this to be
        #     scale_y_continuous(breaks = seq(0, 1,  by =  0.2),
        #                        limits = c(0, 1),
        #                        labels = scales::label_percent()) +
        #     theme_bw() +
        #     theme(axis.text = element_text(size = 14),
        #           strip.text = element_text(size = 12),
        #           axis.title = element_text(size = 18),
        #           legend.text = element_text(size = 18),
        #           axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
        #           legend.position = "bottom") +
        #     facet_wrap(~area_name)
      })
    })
  })

  plots <- rlang::squash(plots)
  if (!is.null(str_save)) {
    # save
    plots <-  gridExtra::marrangeGrob(plots, nrow = 1, ncol = 1)
    ggsave(filename = str_save,
           plot = plots,
           dpi = "retina",
           width = save_width,
           height = save_height)
  } else {
    return(plots)
  }
}

# function to compare DMPTT2 fit to our model fit
plt_dmppt2_compare_fits <- function(circ_data,
                                    dmppt2_data,
                                    age_per,
                                    # by default, take highest area in dmppt2
                                    area_levels =
                                      max(dmppt2_data$area_level,
                                          na.rm = TRUE),
                                    years = NULL,
                                    model_type = NULL,
                                    xlab = "Year",
                                    ylab = "Circumcision Prevalence",
                                    title,
                                    str_save = NULL,
                                    save_width = NULL,
                                    save_height = NULL,
                                    # n_plots = 1) {
                                    n_plots = 1) {

  # circ_data_test <- circ_data
  # dmppt2_data_test <- dmppt2_data
  # circ_data <- circ_data_test
  # dmppt2_data <- dmppt2_data_test
  # age_per <- "15-49"
  # # area_levels = NULL
  # area_levels =
  #     max(dmppt2_data$area_level,
  #         na.rm = TRUE)
  # years <- c(max(dmppt2_data$year), max(2013, min(dmppt2_data$year)))
  #
  # model_type = NULL
  # n_plots = 1
  # title <- xlab <- ylab <- "TEST"
  # save_width = 16
  # save_height = 12

  # by default take highest area in dmppt2
  # if (is.null(area_levels)) {
  #     area_levels <- max(dmppt2_data$area_level, na.rm = TRUE)
  # }

  # perform initial filtering
  cols <- c("age_group", "area_level", "year", "model")
  vals <- list(age_per, area_levels, years, model_type)
  for (i in seq_along(cols)) {
    circ_data <- initial_filter(circ_data, cols[i], vals[[i]])
    dmppt2_data <- initial_filter(dmppt2_data, cols[i], vals[[i]])
  }

  # For LSO, DMPPT2 models medical, not all circumcision
  if (all(circ_data$iso3 == "LSO")) {
    circ_data <- circ_data %>%
      filter(type == "MMC coverage")
  } else {
    circ_data <- circ_data %>%
      filter(type == "MC coverage")
  }

  # join datasets
  circ_data <- circ_data %>%
    left_join((dmppt2_data %>%
                 select(area_id, year, age_group,
                        dmppt2_circumcision_coverage)))

  # replace NAs with "NA" in parent_area_name
  circ_data <- circ_data %>%
    mutate(
      parent_area_name = ifelse(
        is.na(parent_area_name),
        "NA",
        parent_area_name
      )
    )

  # split results by area level, year and number of plots desired (no need
  # for area level)
  # circ_data <- purrr::flatten(
  #   lapply(
  #     split_area_level(circ_data, years = TRUE, n_plots = n_plots),
  #     purrr::flatten
  #   )
  # )
  # don't use n_plots arg here
  circ_data <- purrr::flatten(split_area_level(circ_data, years = TRUE))

  plots <- lapply(seq_along(circ_data), function(i) { # area

    spec_circ_data <- circ_data[[i]]

    # x and y axes labels should be the same, to align abline
    lims <- c(
      min(min(
        spec_circ_data$mean,
        spec_circ_data$dmppt2_circumcision_coverage,
        na.rm = TRUE
      ), na.rm = TRUE),
      max(max(
        spec_circ_data$mean,
        spec_circ_data$dmppt2_circumcision_coverage,
        na.rm = TRUE), na.rm = TRUE)
    )
    # lims <- c(0, 1)

    # get specific title for each plot page
    add_title <- paste(
      spec_circ_data$iso3[1],
      paste0("area level: ", spec_circ_data$area_level[1]),
      spec_circ_data$area_level_label[1],
      spec_circ_data$year[1],
      sep = ", "
    )

    ggplot(spec_circ_data, aes(x = dmppt2_circumcision_coverage)) +
      geom_point(aes(y = mean, col = parent_area_name), size = 3) +
      # geom_errorbar(
      #     aes(ymin = lower, ymax = upper, col = parent_area_name),
      #     width = 0.01,
      #     alpha = 0.5
      # ) +
      # geom_pointrange(aes(
      geom_linerange(aes(
        y = mean, ymin = lower, ymax = upper, col = parent_area_name
      ), alpha = 0.3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      ggrepel::geom_text_repel(
        # aes(y = mean, label = age_group),
        aes(y = mean, label = area_name),
        size = 4,
        # position = dodge,
        show.legend = FALSE,
        max.overlaps = 6
      ) +
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = lims
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = lims
      ) +
      coord_fixed(ratio = 1) +
      labs(x =  xlab, y = ylab, col = "Parent Area") +
      ggtitle(paste0(title, add_title)) +
      theme_bw() +
      # facet_wrap(~ area_name) +
      theme(
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        # axis.text.x = element_text(angle = 90,
        #                            vjust = 0.5,
        #                            size = 10),
        legend.position = "bottom"
      )
  })

  # plots <- purrr::flatten(plots)
  # if (!"data.frame" %in% class(plots[[1]][[1]])) {
  #     plots <- purrr::flatten(plots)
  # }
  #
  if (!is.null(str_save)) {
    # save
    plots <-  gridExtra::marrangeGrob(plots, nrow = 1, ncol = 1)
    ggsave(filename = str_save,
           plot = plots,
           dpi = "retina",
           width = save_width,
           height = save_height)
  } else {
    return(plots)
  }
}

#### Abstract Plot ####

plt_coverage_year_national <- function(
  results_agegroup,
  areas,
  last_surveys,
  spec_age_group,
  spec_years,
  spec_model = "No program data",
  # esa_wca_split = FALSE,
  main = NULL,
  str_save = NULL,
  save_width = NULL,
  save_height = NULL,
  n_plots = 1
) {

  if (length(spec_years) == 2 && (spec_years[2] - spec_years[1]) > 1) {
    message("Changing spec_years to a sequence from the first to last year")
    spec_years <- sort(spec_years)
    spec_years <- spec_years[1]:spec_years[2]
  }

  # Subset results
  tmp <- results_agegroup %>%
    filter(
      area_level == 0,
      type     %in% c("MC coverage", "MMC coverage", "TMC coverage"),
      age_group  == spec_age_group,
      year     %in% spec_years
    ) %>%
    summary_col_renamer(letter = "y") %>%
    # Relabelling for plot
    arrange(desc(mean.y)) %>%
    mutate(
      # type = stringr::str_remove(type, " coverage")# ,
      # area_id = factor(area_id, unique(.data$area_id))
      type = ifelse(grepl("MMC", type),
                    "Medical Male Circumcision (MMC)",
                    ifelse(grepl("TMC", type),
                           "Traditional Male Circumcision (TMC)",
                           "Male Circumcision (MC)"))
    )

  # add last surveys for each country
  tmp <- tmp %>%
    # left_join(last_surveys, by = "area_id") %>%
    left_join(
      distinct(select(last_surveys, -matches("iso3"))),
      by = c("area_id")
    ) %>%
    # add alpha to plot if the circumcisions are projected
    mutate(light = ifelse(year <= survey_year, "Surveyed", "Projected"))
  # pull where year == survey year and add another row for projected for it!
  extra_rows <- tmp %>%
    filter(year == survey_year) %>%
    mutate(light = "Projected")
  tmp <- rbind(tmp, extra_rows) %>%
    arrange(area_id, type, year)

  # add in required columns from areas and order area names
  tmp <- add_area_info(tmp, areas, area_names_from = "data") #

  if (!is.factor(tmp$area_name)) {
    tmp <- tmp %>%
      mutate(
        area_name = case_when(
          grepl("Tanzania", area_name)              ~ "Tanzania",
          area_name == "GHANA"                      ~ "Ghana",
          # area_name == "Congo"                      ~ "Republic of the Congo",
          grepl("Democratic Republic", area_name)   ~ "DR Congo",
          area_name == "Equatorial Guinea"          ~ "Eq. Guinea",
          area_name == "Central African Repulic"    ~ "Cent. Af. Rep.",
          TRUE                                      ~ area_name
        )
      )
  }

  # reorder panels geographically (Roughly West to East, North to South)
  if (!is.factor(tmp$area_name)) {
    ordered_areas <- distinct(tmp, area_id, area_name)
    levs <- c(
      # West Coast and Central as far as Angola
      "SEN", "GMB", "MLI", "BFA", "NER", "GNB", "GIN", "SLE", "LBR", "CIV", "GHA",
      "TGO", "BEN", "NGA", "TCD", "CMR", "CAF", "GNQ", "GAB", "COG", "COD", "AGO",
      # switching to East Coast and Southern
      "ETH", "KEN", "UGA", "TZA", "RWA", "BDI", "MOZ", "MWI", "ZMB", "ZWE", "BWA",
      "NAM", "ZAF", "SWZ", "LSO"
    )

    levs <- levs[levs %in% ordered_areas$area_id]

    ordered_areas$area_id <-  factor(
      ordered_areas$area_id,
      levels = levs
    )
    ordered_areas <- ordered_areas[order(ordered_areas$area_id), ]
    tmp$area_name <- factor(
      tmp$area_name,
      # stringr::str_to_title(tmp$area_name),
      # countrycode::countrycode(tmp$area_id, origin = "iso3c", destination = "country.name"),
      levels = ordered_areas$area_name
    )
  }

  # allow for ESA-WCA Split
  # if (esa_wca_split == TRUE) {
  #   tmp <- tmp %>%
  #     left_join(
  #       select(threemc::esa_wca_regions, -matches("four_region")),
  #       by = "iso3"
  #     )
  # }

  # data for specific total circumcision, to add to plot labels
  tmp1 <- tmp %>%
    filter(type == "Male Circumcision (MC)") %>%
    arrange(area_name)

  dat_all <- tmp %>%
    # plot TMC in front of MC, relabel MC to MMC
    filter(
      type != "Male Circumcision (MC)",
      mean.y > 0
    )

  missing_type_iso3 <- unique(tmp1$iso3)
  missing_type_iso3 <- missing_type_iso3[!missing_type_iso3 %in% unique(dat_all$iso3)]
  if (length(missing_type_iso3) > 0) {
    missing_dat_all <- tmp1 %>%
      filter(iso3 %in% missing_type_iso3) %>%
      mutate(type = "Unknown")
    dat_all <- bind_rows(dat_all, missing_dat_all)
  }

  dat_all <- dat_all %>%
    arrange(area_name) %>%
    mutate(
      light = factor(light, levels = c("Surveyed", "Projected")),
      type = case_when(
        grepl("MMC", type) ~ "MMC",
        grepl("TMC", type) ~ "TMC",
        TRUE               ~ as.character(type)
      ),
      type = forcats::fct_drop(type)
    )

  dat_all <- purrr::flatten(split_area_level(dat_all, n_plots = n_plots))
  tmp1 <- purrr::flatten(split_area_level(tmp1, n_plots = n_plots))

  if (is.null(main)) {
    spec_title <- paste0(
      "Male Circumcision Coverage, ",
      spec_years[1],
      "-",
      last(spec_years),
      " age ",
      spec_age_group,
      " years"
    )
  } else spec_title <- main

  plot_fun <- function(all_data, mc_data, spec_years, spec_age_group, n_plots) {

    manual_fill <- wesanderson::wes_palette("Zissou1", 3)[c(1, 3)]

    # add purple for unknown type, to be in line with map plots
    if ("Unknown" %in% all_data$type) {
      manual_fill <- c(manual_fill, "#5e4fa2")
    }

    all_data %>%
      ggplot(
        aes(
          x = year,
          y = 100 * mean.y,
          alpha = light,
          fill = type
        )
      ) +
      # Adding target line to prevalence
      geom_hline(
        # yintercept = 80,
        yintercept = 90,
        size = 0.5,
        linetype = "dashed",
        colour = "grey50"
      ) +
      # Prevalence as area plot
      geom_area(
        position = "stack",
        data = filter(all_data, light == "Surveyed")
      ) +
      geom_area(
        position = "stack",
        data = filter(all_data, light == "Projected")
      ) +
      # add alpha and associated legend
      scale_alpha_manual(values = c("Surveyed" = 1, "Projected" = 0.5)) +
      guides(
        alpha = guide_legend(override.aes = list(
          fill = wesanderson::wes_palette("Zissou1", 1)
        ))
      ) +
      # annotate plots with coverage at first year
      geom_text(
        data = mc_data %>%
          filter(type == "Male Circumcision (MC)", year == spec_years[1]) %>%
          select(area_id, area_name, year, mean.y),
        aes(x = spec_years[1],
            y = 100 * mean.y,
            label = if_else(year %in% spec_years[1],
                            scales::percent(mean.y, 1),
                            NA_character_)),
        inherit.aes = FALSE,
        fontface = "bold",
        nudge_x = 1,
        colour = "grey30",
        size = 6,
        nudge_y = -1,
        show.legend = FALSE
      ) +
      # annotate plots with coverage at last year
      geom_text(
        data = mc_data %>%
          filter(type == "Male Circumcision (MC)", year == last(spec_years)) %>%
          select(area_id, area_name, year, mean.y),
        aes(
          x = last(spec_years),
          y = 100 * mean.y,
          label = if_else(
            year %in% c(last(spec_years)),
            scales::percent(mean.y, 1),
            NA_character_
          )# ,
          # fill = NA
        ),
        inherit.aes = FALSE,
        fontface = "bold",
        # vjust = 0,
        # vjust = 1,
        nudge_x = -1,
        colour = "grey30",
        # size = 6,
        # size = 17,
        size = 6,
        # nudge_y = 7,
        # nudge_y = 2,
        nudge_y = -1,
        show.legend = FALSE
      ) +
      # Setting for the axes
      scale_x_continuous(
        breaks = seq(spec_years[1], last(spec_years), by = 2),
        limits = c(spec_years[1] - 0.25, last(spec_years) + 0.75)
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(5),
        limits = c(0, 108)
      ) +
      # Setting colour palette
      scale_fill_manual(
        values = manual_fill
      ) +
      # Plotting labels
      ggtitle(spec_title) +
      labs(x = "",
           y = "Circumcision Coverage (%)",
           alpha = "",
           fill = "") +
      # facet_wrap(. ~ area_name, ncol = 4) +
      facet_wrap(. ~ area_name, ncol = ceiling(n_plots / 4)) +
      # Minimal theme
      theme_minimal() +
      # labs(tag = "B") +
      # Altering plot text size
      theme(
        axis.text.y = element_blank(),
        strip.text = element_text(size = 20, face = "bold"),
        strip.background = element_blank(),
        axis.title = element_text(size = 16),
        panel.grid = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.0),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 22)
      )
  }

  recursive_plot_fun <- function(all_data, mc_data, ...) {
    if (!inherits(all_data, "data.frame")) {
      lapply(seq_along(all_data), function(i) {
        recursive_plot_fun(all_data[[i]], mc_data[[i]], ...)
      })
    } else {
      plot_fun(all_data, mc_data, ...)
    }
  }

  # plots <- recursive_plot_fun(dat_all, tmp1, spec_years, spec_age_group, n_plots)
  plots <- lapply(seq_along(dat_all), function(i) {
    recursive_plot_fun(
      dat_all[[i]], tmp1[[i]], spec_years, spec_age_group, n_plots
    )
  })
  plots <- rlang::squash(plots)
  if (!is.null(str_save)) {
    ggsave(
      # filename = paste0("Runs/plots/", cntry, "_Figure1.pdf"),
      filename = str_save,
      plot = gridExtra:: marrangeGrob(plots, nrow = 1, ncol = 1),
      dpi = "retina",
      # width = 16,
      width = save_width,
      # height = 7.5
      height = save_height
    )
  } else {
    return(plots)
  }
}

#### Function to Plot Empirical vs Modelled Rates ####

plt_empirical_model_rates <- function(
    empirical_rates,
    df_results = NULL,
    spec_type,
    spec_age_groups = c(
      "0-4",   "5-9",   "10-14", "15-19", "20-24", "25-29",
      "30-34", "35-39", "40-44", "45-49", "50-54", "54-59"
    ),
    spec_area_levels = NULL,
    spec_years = NULL,
    facet_var = "age_group",
    take_log = FALSE,
    split_areas = TRUE,
    xlab = NULL,
    ylab = NULL,
    title = NULL,
    str_save = NULL,
    save_width = 9,
    save_height = 7
) {

  cols <- sort(c("type", "age_group", "area_level", "year"))
  # pull values in arguments
  vals <- as.list(environment())
  vals <- vals[names(vals) %in% names(formals(plt_empirical_model_rates))]
  vals <- vals[grepl("spec", names(vals))]
  vals <- vals[order(names(vals))]

  # function to preprocess data for plotting
  preprocess_fun <- function(.data, spec_age_groups, take_log, split_areas) {
    # filter for specified type, age_group(s), area_level and year(s)
    .data <- initial_filter_recursive(
      .data, cols, vals
    )
    # Order age groups (make all of this into "prep" function)
    .data$age_group <- factor(
      .data$age_group, levels = spec_age_groups
    )
    .data$year <- as.factor(.data$year)

    if (take_log) {
      last_num_col <- "mean"
      if ("upper" %in% names(.data)) last_num_col <- "upper"
      .data <- .data %>%
        mutate(
          across(matches("mean"):matches(last_num_col), log),
          across(
            matches("mean"):matches(last_num_col),
            ~ ifelse(is.infinite(.), NA, .)
          )
        )
    }
    # split by area level and number of areas desired in each plot
    if (split_areas) .data <- split_area_level(.data, n_plots = 1) else .data
  }

  empirical_rates_plt <- preprocess_fun(
    empirical_rates, spec_age_groups, take_log, split_areas
  )
  if (!is.null(df_results)) {
    df_results_plt <- preprocess_fun(
      df_results, spec_age_groups, take_log, split_areas
    )
    dot_col <- "black"
  } else {
    message("No model estimates provided, only showing empirical rates")
    df_results_plt <- NULL
    dot_col <- "red"
  }

  if (!facet_var %in% c("age_group", "year")) {
    stop("please specify 'facet_var' as one of of 'age_group' or 'year'")
  }

  x_var <- c("age_group", "year")
  x_var <- x_var[x_var != facet_var]

  # function for creating individual plots
  plot_fun <- function(empirical_rates, df_results = NULL) {
    # start with black dots for empirical rates
    p <- ggplot(
      empirical_rates,
      # aes(x = year, y = mean)
      aes(x = .data[[x_var]], y = mean)
    ) +
      geom_point(
        colour = dot_col,
        size = 2
      )

    # if desired, add model estimates with associated error bounds
    if (!is.null(df_results)) {
      p <- p +
        geom_ribbon(
          data = df_results,
          aes(
            # x     = year,
            x     = .data[[x_var]],
            y     = mean,
            ymin  = lower,
            ymax  = upper,
            fill  = .data[[facet_var]],
            group = .data[[facet_var]]
          ),
          alpha = 0.7
        ) +
        geom_line(
          data = df_results,
          aes(
            # x      = year,
            x      = .data[[x_var]],
            y      = mean,
            colour = age_group,
            group  = age_group
          ),
          size = 1
        )
    }
    # facet and format plot
    if (!take_log) {
      p <- p +
        scale_y_continuous(
          # breaks = seq(0, 1, by = 0.2),
          # limits = c(0, 1),
          labels = scales::label_percent()
        )
    }

    # if (facet_var == "age_group") {
    #   p <- p +
    #     scale_x_continuous(
    #       breaks = seq(spec_years[1], last(spec_years), by = 2)
    #     )
    # }

    p <- p +
      # facet_wrap(~ age_group) +
      facet_wrap(~ .data[[facet_var]]) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "none"
      )

    # label plot
    if (!is.null(xlab)) p <- p + xlab(xlab)
    if (!is.null(ylab)) p <- p + ylab(ylab)
    main <- paste0(
      empirical_rates$area_name[[1]],
      ", area level ", empirical_rates$area_level[1]
    )
    if (!is.null(title)) main <- paste0(title, main)
    p <- p + ggtitle(main)
  }

  # function which creates plots recursively for each split
  recursive_plot_fun <- function(empirical_rates, model_rates, ...) {
    if (!inherits(empirical_rates, "data.frame")) {
      lapply(seq_along(empirical_rates), function(i) {
        if (!is.null(model_rates)) model_rates <- model_rates[[i]]
        recursive_plot_fun(empirical_rates[[i]], model_rates, ...)
      })
    } else {
      plot_fun(empirical_rates, model_rates, ...)
    }
  }

  plots <- recursive_plot_fun(empirical_rates_plt, df_results_plt)

  # save plots, if desired
  if (inherits(plots, "list")) plots <- rlang::squash(plots)
  if (!is.null(str_save)) {
    ggsave(
      filename = str_save,
      plot = gridExtra::marrangeGrob(
        plots, nrow = 1, ncol = 1
      ),
      dpi = "retina",
      width = save_width,
      height = save_height
    )
  } else {
    return(plots)
  }
}


#### OOS & Investigating Variance ####

# plots vs year, should allow it to plot vs age group as well!
threemc_val_plt <- function(
    df_results_oos,
    df_results_orig = NULL,
    df_results_survey = NULL,
    all_surveys = NULL,
    spec_agegroup,
    spec_years = unique(df_results_oos$year),
    spec_type,
    # take_log = TRUE,
    spec_area_level = unique(df_results_oos$area_level),
    x_var = "year",
    colour_var = NULL,
    take_log = FALSE,
    facet = "grid",
    facet_formula = formula(~ area_name),
    xlab, ylab, title,
    str_save = NULL, save_width = 16, save_height = 12,
    n_plots = 12
) {

  # browser()

  # df_results_oos <- results_oos_val
  # df_results_orig <- results_agegroup_comparison
  # df_results_survey <- survey_data
  # df_results_survey <- NULL
  # # all_surveys_test = all_surveys
  # # all_surveys = all_surveys_test
  # spec_agegroup <- "15-49"
  # spec_years <- unique(results_oos_val$year)
  # spec_type <- "MMC coverage"
  # spec_area_level <- 0
  # take_log <- FALSE
  # xlab <- "test"
  # ylab <- "test"
  # title <- "test"
  # n_plots <- 12

  stopifnot(x_var %in% c("year", "age_group"))

  if (!is.null(df_results_orig) && !"indicator" %in% names(df_results_orig)) {
    df_results_oos <- bind_rows(
      mutate(df_results_oos, indicator = "OOS"),
      mutate(df_results_orig, indicator = "Original"),
    )
  } else if (!is.null(df_results_orig)) {
    df_results_oos <- bind_rows(df_results_oos, df_results_orig)
  }

  # filter data accordingly (need to add more arguments to this)
  initial_filter <- function(.data, type_filter) {
    .data <- .data %>%
      filter(
        year %in% spec_years,
        area_level %in% spec_area_level,
        age_group %in% spec_agegroup,
        type %in% type_filter
      )
    # if ("model" %in% names(.data))
    #     .data <- .data[.data$model == model_type, ]
    return(.data)
  }
  df_results_oos <- initial_filter(df_results_oos, spec_type)
  if (!is.null(df_results_survey)) {
    df_results_survey <- initial_filter(df_results_survey, spec_type)

    # make sure areas are the same for both
    df_results_survey <- df_results_survey %>%
      # filter(area_name %in% df_results_oos$area_name)
      filter(area_id %in% df_results_oos$area_id)
  }

  # reset df_results_survey to NULL if it has no rows
  if (!is.null(df_results_survey) && nrow(df_results_survey) == 0) {
      df_results_survey <- NULL
  } # else if ("type" %in% names(df_results_survey) && !"indicator" %in% names(df_results_survey)) {
    #   df_results_survey <- rename(df_results_survey, indicator = type)
  # }

  # convert to the log scale if desired
  if (take_log == TRUE) {
    df_results_oos <- df_results_oos %>%
      mutate(across(mean:upper, log))
    if (!is.null(df_results_survey)) {
      df_results_survey <- df_results_survey %>%
        mutate(across(mean:upper, log))
    }
  }

  # add alpha/shape column for aggregations and surveys
  if (!is.null(all_surveys) && x_var == "year") {
    all_surveys_orig <- all_surveys %>%
      filter(area_id %in% df_results_oos$area_id) %>%
      # filter(iso3 %in% df_results_oos$area_id) %>%
      filter(survey_year == max(survey_year)) %>%
      slice(1)

    # add "light" column to aggregation and survey results
    df_results_oos <- add_last_surveys(df_results_oos, all_surveys_orig, add_rows = TRUE)
    if(!is.null(df_results_survey)) {
      df_results_survey <- add_last_surveys(df_results_survey, all_surveys_orig, add_rows = TRUE)
    }
  } else {
    df_results_oos$light <- "Surveyed"
    if(!is.null(df_results_survey)) {
      df_results_survey$light <- "Surveyed"
      df_results_survey$indicator <- "Surveyed"
    }
  }

  # Ordering age groups
  if (x_var == "age_group") {
    df_results_oos$age_group <- as.numeric(factor(
      df_results_oos$age_group, levels = spec_agegroup
    ))
    if (!is.null(df_results_survey)) {
      df_results_survey$age_group <- as.numeric(factor(
        df_results_survey$age_group, levels = spec_agegroup
      ))
    }
  }

  # split results by area level, and number of plots desired
  df_results_oos <- split_area_level(df_results_oos, n_plots = n_plots)
  if (!is.null(df_results_survey)) {
    df_results_survey <- split_area_level(df_results_survey, n_plots = n_plots)
  }

  # plot for each (nested) loop
  plots <- lapply(seq_along(df_results_oos), function(i) {
    lapply(seq_along(df_results_oos[i]), function(j) {
      plt_data1 <- df_results_oos[[i]][[j]]
      if (!is.null(df_results_survey)) {
        plt_data2 <- df_results_survey[[i]][[j]]
        # plt_data1$alpha <- plt_data2$alpha <- 0.8
      } # else {
      # plt_data1$alpha <- ifelse(plt_data1$light == "Surveyed", 0.8, 0.3)
      # }

      # browser()

      if (i == 1 && j == 1) {
        plt_data1$parent_area_name <- NA_character_
      }

      # get specific title for each plot page
      add_title <- paste(
        plt_data1$iso3[1],
        plt_data1$area_level[1],
        plt_data1$area_level_label[1],
        sep = ", "
      )

      if (is.null(colour_var) && "indicator" %in% names(plt_data1)) {
        colour_var <- "indicator"
      } else if (is.null(colour_var)) {
        colour_var <- "parent_area_id"
      }

      # p <- ggplot(plt_data1, aes(x = .data[[x_var]])) +
      if (x_var == "year") {
          p <- ggplot(plt_data1, aes(x = .data[[x_var]]))
      } else if (x_var == "age_group") {
          p <- ggplot(plt_data1, aes(x = .data[[x_var]], group = as.factor(.data[[colour_var]])))
      }
      p <- p +
        # Modelled rate
        geom_line(
          aes(y = mean, col = as.factor(.data[[colour_var]])),
          size = 1
        ) +
        # Credible interval
        geom_ribbon(
          data = filter(plt_data1, light == "Surveyed"),
          aes(ymin = lower, ymax = upper, fill = as.factor(.data[[colour_var]]), alpha = light)
        ) +
        geom_ribbon(
          data = filter(plt_data1, light == "Projected"),
          aes(ymin = lower, ymax = upper, fill = as.factor(.data[[colour_var]]), alpha = light)
        )

      if (!is.null(df_results_survey)) {
        p <- p +
          # survey points (want clear points for surveys not included)
          geom_pointrange(
            data = filter(plt_data2, light == "Surveyed"),
            # aes(y = mean, ymin = lower, ymax = upper, colour = as.factor(.data[[colour_var]])),
            aes(y = mean, ymin = lower, ymax = upper),
            colour = "black",
            # size = 0.3,
            show.legend = FALSE
          ) +
          geom_pointrange(
            data = filter(plt_data2, light == "Projected"),
            # aes(y = mean, ymin = lower, ymax = upper, colour = as.factor(.data[[colour_var]])),
            aes(y = mean, ymin = lower, ymax = upper),
            shape = 1,
            colour = "black",
            # size = 0.3,
            show.legend = FALSE
          )  # +
        # guides(alpha = "none")
      }

      # if (x_var == "year") {
        p <- p +
          scale_alpha_manual(
            values = c("Surveyed" = 0.8, "Projected" = 0.3)
          )
      # } else if (x_var == "age_group") {

      p <- p +
        guides(
          alpha = guide_legend(override.aes = list(
            fill = wesanderson::wes_palette("Zissou1", 1)
          ))
        ) +
        # Labels
        labs(
          x = xlab,
          y = ylab,
          colour = "",
          fill = "",
          alpha = "Period"
        ) +
        ggtitle(paste0(title, add_title))

      # remove alpha legend
      if (nrow(filter(plt_data1, light == "Projected")) == 0) {
        p <- p + guides(alpha = "none")
      }

      if (x_var == "age_group") {
        p <- p +
          scale_x_continuous(
            breaks = 1:length(spec_agegroup), # (length(age_per) + 1),
            labels = spec_agegroup # c(age_per, final_label),
        )
      }

      if (x_var == "year") {
          p <- p +
              scale_x_continuous(
                  breaks = seq(min(plt_data1$year), max(spec_years), by = 2)
              )
      }

      if (all(grepl(paste("coverage", "probability", sep = "|") , spec_type)) && take_log == FALSE) {
        p <- p +
        scale_y_continuous(
          breaks = seq(0, 1, by = 0.25),
          limits = c(0, 1),
          label = scales::label_percent(accuracy = 1)
        )
      }
      p <- p +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "bottom"
        )

      if (!is.null(facet) && facet != FALSE) {
        if (facet == "wrap") {
          p <- p + facet_wrap(facet_formula)
        } else if (facet == "grid") {
          p <- p + facet_grid(facet_formula)
        }
      }

      return(p)
    })
  })

  # flatten nested list of plots
  plots <- purrr::flatten(plots)
  if (!is.null(str_save)) {
    # save
    plots <-  gridExtra::marrangeGrob(plots, nrow = 1, ncol = 1)
    ggsave(filename = str_save,
           plot = plots,
           dpi = "retina",
           width = save_width,
           height = save_height)
  } else {
    return(plots)
  }
}
