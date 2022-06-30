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

  # load files and append them together. Return this appended df
  results_age <- as.data.frame(rbindlist(
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
add_last_surveys <- function(.data, last_surveys, add_rows = TRUE) {
    # join data with last_surveys
    if (!"iso3" %in% names(.data)) {
        .data$iso3 <- substr(.data$area_id, 0, 3)
    }
    .data <- .data %>%
        # add last surveys for each country
        left_join(last_surveys, by = c("iso3", "area_id")) %>%
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


# circumcision coverage, split by type, across different discrete ages #
plt_age_coverage_by_type <- function(
  results_age, areas, spec_years, area_levels, spec_model,
  str_save = NULL, save_width, save_height, n_plots = 1
  ) {
  # Subsetting
  tmp1 <- results_age %>%
    filter(
      type %in%     c("MC probability", "MMC probability", "TMC probability",
                      "MC coverage",    "MMC coverage",    "TMC coverage"),
      # year %in%     c(2009, 2015, 2021)# ,
      year %in% spec_years,
      area_level %in% area_levels,
      model == spec_model
    ) %>%
    # rename columns
    summary_col_renamer() %>%
    dplyr::mutate(
      # multiply prevalence by 100
      across(c(mean.x, lower.x, upper.x), ~ ifelse(type %like% "coverage",
                                                   . * 100,
                                                   .)),
      # relabel for plot
      type1 = ifelse(type %like% "coverage", "Y", "Z"),
      type2 = ifelse(type %like% "MMC", "B",
                     ifelse(type %like% "TMC", "C", "A")) # A for MC
    )

  # Dummy dataset for limits in each panel
  dummy1 <- rbind(expand.grid(x = c(0, 60),
                              y = c(0, 0.2),
                              year = NA,
                              type2 = c("A", "B", "C"),
                              type1 = "Z"),
                  expand.grid(x = c(0, 60),
                              y = c(0, 100),
                              year = NA,
                              type2 = c("A", "B", "C"),
                              type1 = "Y"))

  # Dummy dataset to add 80% target
  dummy2 = data.frame(type2 = c("A", "B", "C"),
                      type1 = "Y",
                      year = NA,
                      y = c(80, 80, 80))

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
        b$area_level[1],
        b$area_level_label[1],
        sep = ", "
      )
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
        scale_x_continuous(breaks = seq(0, 60, by = 5)) +
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
      plot = gridExtra:: marrangeGrob(purrr::flatten(plots), nrow = 1, ncol = 1),
      dpi = "retina",
      # width = 15,
      width = save_width,
      # height = 11
      height = save_height
    )
  } else {
    return(purrr::flatten(plots))
  }
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
      left_join(areas, by = c("area_id", "area_name"))
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
add_area_info <- function(.data, areas) {

  if ("sf" %in% class(areas)) areas <- st_drop_geometry(areas)
  areas_join <- areas %>%
    dplyr::select(
      iso3,       area_id,          area_name,
      area_level, area_level_label, area_sort_order
    ) %>%
    distinct()

  left_join(.data, areas_join)
}

#### split_area_level ####
# split results by area level (and optionally by year), and then again so
# only the number of plots desired on each page is displayed
split_area_level <- function(.data, years = FALSE, n_plots = NULL) {

  # find unique areas. Only want "n_plots" areas on each page of the plot
  if (!is.null(n_plots)) {
      distinct_areas <- .data %>%
          distinct(area_name, area_level) %>%
          group_by(area_level) %>%
          dplyr::mutate(split = ceiling(row_number() / n_plots)) %>%
          ungroup()
      .data <- .data %>%
          left_join(distinct_areas, by = c("area_name", "area_level"))
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

  # Subsetting
  tmp1 <- results_agegroup %>%
    filter(
      # type %in%     c("MMC-nTs performed", "MMC-Ts performed", "TMCs performed"),
      type %in% c("MMCs performed", "TMCs performed"),
      # age_group ==  "10+",
      age_group == spec_age_group,
      # year %in%     c(2009:2021),
      year %in% spec_years,
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
        type = ifelse(type %like% "MMC", "MMC", "TMC")
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

  # Dummy dataset to add 80% target
  dummy2 = data.frame("test" = c("A", "B"),
                      "type" = NA,
                      "y"    = c(80, NA))

  # split tmp by area level and number of desired plots
  tmp <- split_area_level(tmp, n_plots = n_plots)

  plots <- lapply(tmp, function(a) {
    lapply(a, function(b) {

      # title displaying area level and label
      spec_title <- paste(
        b$iso3[1],
        b$area_name[1],
        b$area_level[1],
        b$area_level_label[1],
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
        scale_x_continuous(breaks = seq(first(spec_years),
                                        last(spec_years), by = 1)) +
        scale_y_continuous(breaks = scales::pretty_breaks(8),
                           limits = c(0, NA)) +
        # Setting colour palette
        scale_fill_manual(
          values = wesanderson::wes_palette("Zissou1", 3)[c(1, 3)]) +
        # Plotting labels
        labs(x = "Year",
             y = "",
             fill = "") +
        # Minimal theme
        theme_bw() +
        ggtitle(spec_title) +
        # Facet wrapping
        # facet_wrap(. ~ test ,
        facet_wrap(vars(test),
                   strip.position = "left",
                   scales = "free",
                   labeller = as_labeller(c(
                     A = "Circumcision coverage (%)",
                     # B = "Number of circumcisions performed (in 1000s)"))) +
                     B = "Number of circumcisions performed"))) +
        # Extra options on the plot
        theme(axis.text = element_text(size = 15),
              strip.text = element_text(size = 16),
              strip.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              legend.text = element_text(size = 16),
              axis.title = element_text(size = 18),
              plot.title = element_text(size = 26, hjust = 0.5),
              strip.placement = "outside",
              legend.position = "bottom")
    })
  })

  if (!is.null(str_save)) {
    ggsave(
      # filename = paste0("Runs/plots/", cntry, "_Figure1.pdf"),
      filename = str_save,
      plot = gridExtra:: marrangeGrob(purrr::flatten(plots), nrow = 1, ncol = 1),
      dpi = "retina",
      # width = 16,
      width = save_width,
      # height = 7.5
      height = save_height
    )
  } else {
    return(purrr::flatten(plots))
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
      across(c(mean.x, lower.x, upper.x), ~ ifelse(type %like% "coverage",
                                                   . * 100,
                                                   .)),
      # relabel for plot
      type1 = ifelse(type %like% "coverage", "Y", "Z"),
      type2 = ifelse(type %like% "MMC", "B",
                     ifelse(type %like% "TMC", "C", "A")) # A for MC
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

  # Dummy dataset to add 80% target
  dummy2 = data.frame(type2 = c("A", "B", "C"),
                      type1 = "Y",
                      year = NA,
                      y = c(80, 80, 80))

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
        b$area_level[1],
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
      plot = gridExtra:: marrangeGrob(purrr::flatten(plots), nrow = 1, ncol = 1),
      dpi = "retina",
      # width = 15,
      width = save_width,
      # height = 11
      height = save_height
    )
  } else {
    return(purrr::flatten(plots))
  }
}

# Plot Map (fig 3). Works for multiple or single countries. Multiple can be either as
# a single plot of SSA (only way currently supported), or faceted by country
plt_coverage_map <- function(
    results_agegroup, areas, colourPalette, spec_age_group, spec_years,
    spec_model, plot_type, # = c("single", "map", "split"),
    spec_countries = NULL, results_area_level = NULL, country_area_level = NULL,
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

    # filter for desired country
    if (!is.null(spec_countries)) {
        areas <- areas %>%
            filter(iso3 %in% spec_countries)
        results_agegroup <- results_agegroup %>%
            filter(area_id %like% paste(spec_countries, collapse = "|"))
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
            filter(area_level <= results_area_level)
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
            type = ifelse(type %like% "MMC", "Medical",
                          ifelse(type %like% "TMC", "Traditional", "Total"))
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

    if (plot_type != "single") {
        # if bb_box is too far West (likely due to Cabo Verde(?)), adjust
        # could have plot dimensions as input??
        bb <- st_bbox(areas_plot)
        if (bb[[1]] < -20) {
            areas_plot <- st_crop(areas_plot, xmin = -20, ymin = bb[[2]],
                                  xmax = bb[[3]], ymax = bb[[4]])
        }
    }

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
                                 limits = c(0,1),
                                 label = scales::label_percent(accuracy = 1),
                                 guide = guide_colorbar(label = TRUE,
                                                        draw.ulim = TRUE,
                                                        draw.llim = TRUE,
                                                        frame.colour = "black",
                                                        ticks = TRUE,
                                                        barheight = 1,
                                                        barwidth = 30)) +
            theme_minimal() +
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  strip.text = element_text(size = 18),
                  legend.text = element_text(size = 14),
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
            map_plot(x, areas_plot, colourPalette) +
                ggtitle(
                    paste(x$iso3[1],
                          x$type[1],
                          "Circumcision Coverage,",
                          x$age_group[1],
                          "years old")
                )
        })
    } else if (plot_type == "map") {
        # create plot for each specific type
        plots <- lapply(tmp1, function(x) {
            # change year to factor, to change facet titles
            x$year <- factor(x$year,
                             labels = paste(unique(x$type),
                                            "Circumcision",
                                            unique(x$year),
                                            "-",
                                            unique(x$age_group),
                                            "year olds"))
            map_plot(x, areas_plot, colourPalette)
        })
    } else if (plot_type == "split") {

        plots <- lapply(tmp1, function(a) {
            lapply(a, function(b) {
                spec_areas <- filter(areas_plot, iso3 %in% b$iso3)
                p <- map_plot(b, spec_areas, colourPalette)
                # add title to plots
                p <- p +
                    ggtitle(
                        paste(
                            b$iso3[1],
                            ifelse(b$type[1] == "Total", "MC",
                                   ifelse(b$type[1] == "Medical",
                                          "MMC", "TMC")),
                            # paste(unique(b$year), collapse = " & "),
                            "-",
                            b$age_group[1],
                            "years old"
                        )
                    )
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

            # title <- ggdraw() +
            #     draw_label(
            #         paste(
            #             plots[[i]][[1]]$data$type[1],
            #             "Circumcision -",
            #             plots[[i]][[1]]$data$age_group[1],
            #             "years old"
            #         ),
            #         fontface = "bold",
            #         size = 14,
            #         # fontfamily = "arial",
            #         x = 0,
            #         hjust = -1.2
            #     )

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
        plots <- rlang::flatten(plots)
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
    spec_model = "No program data",
    str_save = NULL,
    spec_title = paste0("Male Circumcision Coverage, ",
                        spec_years[1], "-", last(spec_years),
                        " age ", spec_age_group, " years"),
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
            type = ifelse(type %like% "MMC",
                          "Medical Male Circumcision (MMC)",
                          ifelse(type %like% "TMC",
                                 "Traditional Male Circumcision (TMC)",
                                 "Medical Circumcision (MC)")),
            area_name = ifelse(area_name %like% "Tanzania",
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
        filter(type == "Medical Circumcision (MC)")
    tmp <- tmp %>%
        filter(type != "Medical Circumcision (MC)") %>%
        plyr::rbind.fill()

    # split by area level and number of desired plots
    tmp1 <- split_area_level(tmp1, n_plots = n_plots)
    tmp <- split_area_level(tmp, n_plots = n_plots)

    # plot
    plots <- lapply(seq_along(tmp), function(i) {
        lapply(seq_along(tmp[[i]]), function(j) {

            dat <- tmp[[i]][[j]]
            dat1 <- tmp1[[i]][[j]]

            plot_title <- paste(
                spec_title,
                tmp[[i]][[j]]$iso3[1],
                tmp[[i]][[j]]$area_level[1],
                tmp[[i]][[j]]$area_level_label[1],
                sep = ", "
            )

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
                                           color = "grey30",
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
                                           color = "grey30",
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
                    panel.grid = element_blank(),
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
            plot = gridExtra:: marrangeGrob(purrr::flatten(plots),
                                            nrow = 1,
                                            ncol = 1),
            dpi = "retina",
            width = save_width,
            height = save_height
        )
    } else {
        return(purrr::flatten(plots))
    }
}

# fig 5 (for one type, plot age vs coverage %, for multiple years and
# including error bounds)
plt_age_coverage_multi_years <- function(
    results_age,
    areas,
    spec_type = "MC coverage",
    spec_years, # = c(2009, 2015, 2021)
    area_levels = unique(results_age$area_level),
    spec_model = "No program data",
    spec_ages = c(0, 60),
    str_save = NULL,
    save_width = 9,
    save_height = 7,
    n_plots = 12
) {

    # Subsetting
    tmp <- results_age %>%
        filter(
            type      == spec_type,
            year     %in% spec_years,
            area_level %in% area_levels,
            model == spec_model
        ) %>%
        # multiply prevalence by 100
        mutate(across(c(mean, lower, upper), ~ . * 100))

    # add in required columns from areas and order area names (make function)
    tmp <- add_area_info(tmp, areas)

    # split by area level and number of desired plots
    tmp <- split_area_level(tmp, n_plots = n_plots)

    plots <- lapply(tmp, function(i) {
        lapply(i, function(j) {

            # title displaying area level and label
            spec_title <- paste(
                j$iso3[1],
                j$area_level[1],
                j$area_level_label[1],
                sep = ", "
            )

            ggplot(j,
                   aes(x = age,
                       group = as.factor(year),
                       fill = as.factor(year),
                       colour = as.factor(year))) +
                # Adding target line to prevalence
                geom_hline(yintercept = 80,
                           size = 2,
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
                facet_wrap(. ~ area_name) +
                # Altering plot text size
                theme(axis.text = element_text(size = 16),
                      strip.text = element_text(size = 16),
                      strip.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.title = element_text(size = 20),
                      # plot.title = element_text(size = 40, hjust = 0.5),
                      plot.title = element_text(size = 20, hjust = 0.5),
                      legend.text = element_text(size = 20),
                      legend.position = "bottom")
        })
    })
    if (!is.null(str_save)) {
        ggsave(
            filename = str_save,
            plot = gridExtra:: marrangeGrob(purrr::flatten(plots), nrow = 1, ncol = 1),
            dpi = "retina",
            width = save_width,
            height = save_height
        )
    } else {
        return(purrr::flatten(plots))
    }
}

# fig 6, plotting distributions/ridges for mean circ age for TMIC and MMC-nT
# for different areas
plt_circ_age_ridge <- function(
    results_age,
    areas,
    spec_year = last(results_age$year),
    area_levels = unique(results_age$area_level),
    spec_model = "No program data",
    max_age = 30,
    str_save = NULL,
    save_width = 9,
    save_height = 7,
    n_plots = 8
) {

    # Keeping relevant information
    tmp <- results_age %>%
        # Only keeping relevant data
        filter(
            type %in% c("MMC-nTs performed", "TMICs performed") ,
            area_level %in% area_levels,
            model == spec_model,
            year == spec_year,
            age <= max_age
            # area_level %in% c(0:1)# ,
            # model == "With program data"
        )

    # Getting density for the ridge plot
    tmp <- tmp %>%
        # Grouping for normalising
        group_by(area_id, year, type) %>%
        # Estimating density
        mutate(density = mean /(2 * sum(mean))) %>%
        ungroup() %>%
        # Altering labels for the plot
        mutate(type = ifelse(type %like% "MMC-nT", "Medical", "Traditional")) %>%
        order_area_name()

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
    tmp <- split_area_level(tmp, n_plots = n_plots)
    tmp2 <- split_area_level(tmp2, n_plots = n_plots)

    plots <- lapply(seq_along(tmp), function(i) {
        lapply(seq_along(tmp[[i]]), function(j) {

            plt_data <- tmp[[i]][[j]]

            spec_title <- paste(
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
                       color = type)) +
                geom_density_ridges(stat = "identity",
                                    scale = 1,
                                    alpha = 0.7,
                                    color = NA)  +
                # Adding average age of circumcision
                geom_point(data = tmp2[[i]][[j]],
                           aes(x = average_age,
                               y = as.integer(area_name) - 0.05,
                               color = type),
                           inherit.aes = FALSE,
                           show.legend = FALSE) +
                # Adding uncertainty interval of average age of circumcision
                geom_segment(data =tmp2[[i]][[j]],
                             aes(x = average_age_lower,
                                 xend = average_age_upper,
                                 y = as.integer(area_name) - 0.05,
                                 yend = as.integer(area_name) - 0.05,
                                 color = type),
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
                     color = NULL,
                     fill = NULL) +
                # Changing plot themes
                theme(
                    axis.title = element_text(size = 24),
                    axis.text = element_text(size = 16),
                    plot.title = element_text(size = 40, hjust = 0.5),
                    strip.text = element_text(size = 16),
                    strip.background = element_blank(),
                    legend.title = element_text(size = 16),
                    legend.text = element_text(size = 24),
                    legend.position = 'bottom',
                    panel.spacing = unit(0.2, "lines")
                )
        })
    })

    if (!is.null(str_save)) {
        ggsave(
            filename = str_save,
            plot = gridExtra:: marrangeGrob(purrr::flatten(plots), nrow = 1, ncol = 1),
            dpi = "retina",
            width = save_width,
            height = save_height
        )
    } else {
        return(purrr::flatten(plots))
    }
}

#### Plots for comparing survey points with model fit ####
plt_MC_modelfit_spec_age <- function(df_results, df_results_survey, mc_type_model,
                                  mc_type_survey, age_per, years, model_type,
                                  # area_level_select,
                                  xlab, ylab, title, str_save,
                                  save_width, save_height, n_plots = 12) {

  # filter data accordingly
  initial_filter <- function(.data, type_filter) {
    .data <- .data %>%
      filter(
        year %in% years,
        # area_level == area_level_select,
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
      filter(area_name %in% df_results$area_name)

  # split results by area level, and number of plots desired
  df_results <- split_area_level(df_results, n_plots = n_plots)
  df_results_survey <- split_area_level(df_results_survey, n_plots = n_plots)

  # plot for each (nested) loop
  plots <- lapply(seq_along(df_results), function(i) {
    lapply(seq_along(df_results[i]), function(j) {

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

      ggplot(plt_data1, aes(x = year)) +
      # ggplot(df_results[[i]], aes(x = year)) +
        # Credible interval
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = parent_area_name),
                    # colour  = NA,
                    alpha = 0.5) +
        # Modelled rate
        geom_line(aes(y = mean, col = parent_area_name), size = 1) +
        # geom_point(data = plt_data2,
        #            aes(y = p_ind),
        #            colour = 'black',
        #            show.legend = FALSE) +
        geom_pointrange(
            data = plt_data2,
            aes(y = mean, ymin = lower, ymax = upper),
            colour = "black",
            show.legend = FALSE
        ) +
        # Labels
        labs(x = xlab, y = 'Circumcision prevalence', colour = '', fill = '') +
        ggtitle(paste0(title, add_title)) +
        # Faceting by area
        facet_wrap(~area_name) +
        scale_x_continuous(breaks = seq(min(plt_data1$year),
        # scale_x_continuous(breaks = seq(min(df_results[[i]]$year),
                                        2021,
                                        by = 2)) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                           limits = c(0, 1),
                           label = scales::label_percent(accuracy = 1)) +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
              legend.position = "none")
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

plt_MC_modelfit <- function(df_results, df_results_survey, mc_type_model,
                            mc_type_survey, age_per, survey_years, model_type,
                            # area_level_select,
                            xlab, ylab, title,
                            str_save, save_width, save_height, n_plots = 12) {
  # Preparing dataset for plots
  initial_filter <- function(.data, spec_type) {
    .data <- .data %>%
      filter(
        type == spec_type,
        age_group %in% age_per,
        year %in% survey_years
      )
    if ("model" %in% names(.data)) .data <- .data[.data$model == model_type, ]
    return(.data)
  }
  tmp1 <- initial_filter(df_results, mc_type_model)
  tmp2 <- initial_filter(df_results_survey, mc_type_survey)

  # Ordering age groups (needed??)
  tmp1$age_group <- as.numeric(factor(tmp1$age_group, levels = age_per))
  tmp2$age_group <- as.numeric(factor(tmp2$age_group, levels = age_per))

  # make sure areas are the same for both
  tmp2 <- filter(tmp2, area_name %in% tmp1$area_name)
  tmp1 <- filter(tmp1, year %in% tmp2$year) # make sure years are the same

  # split results by area level, year and number of plots desired
  tmp1 <- split_area_level(tmp1, year = T, n_plots = n_plots)
  tmp2 <- split_area_level(tmp2, year = T, n_plots = n_plots)

  # plot for each (nested) loop
  plots <- lapply(seq_along(tmp1), function(i) { # area
    lapply(seq_along(tmp1[[i]]), function(j) { # year
      lapply(seq_along(tmp1[[i]][[j]]), function(k) { # cap number of plots

        plt_data1 <- tmp1[[i]][[j]][[k]]
        plt_data2 <- tmp2[[i]][[j]][[k]]

        if (i == 1 && j == 1 && k == 1) {
            plt_data1$parent_area_name <- "NA"
        }

        # get specific title for each plot page
        add_title <- paste(
          plt_data1$iso3[1],
          plt_data1$area_level[1],
          plt_data1$area_level_label[1],
          plt_data1$year[1],
          sep = ", "
          )

        ggplot(plt_data1, aes(x = age_group)) +
          geom_ribbon(aes(ymin = lower, ymax = upper, fill = parent_area_name),
                      alpha = 0.75
                      # colour = NA,
                      # fill = 'darkgrey'
          ) +
          geom_line(aes(y = mean, col = parent_area_name),
                    size = 1
                    # colour = 'black'
          ) +
          # geom_point(data = plt_data2,
          #            aes(y = p_ind),
          #            colour = 'black',
          #            show.legend = FALSE) +
          geom_pointrange(
              data = plt_data2,
              aes(y = mean, ymin = lower, ymax = upper),
              colour = "black",
              show.legend = FALSE
          ) +
          # Labels
          labs(x =  xlab, y = ylab, colour = '', fill = '') +
          ggtitle(paste0(title, add_title)) +
          scale_x_continuous(breaks = 1:14,
                             labels = c('0-4','5-9','10-14','14-19','20-24','25-29',
                                        '30-34','35-39','40-44','45-49','50-54',
                                        '55-59','60-64','65+')) + # should change this to be
          scale_y_continuous(breaks = seq(0, 1,  by =  0.2),
                             limits = c(0, 1),
                             labels = scales::label_percent()) +
          theme_bw() +
          theme(axis.text = element_text(size = 14),
                strip.text = element_text(size = 12),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 18),
                axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
                legend.position = 'bottom') +
          facet_wrap(~area_name)
      })
    })
  })

  # flatten nested list of plots
  plots <- purrr::flatten(plots)
  if (!"data.frame" %in% class(plots[[1]][[1]])) {
    plots <- purrr::flatten(plots)
  }

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
