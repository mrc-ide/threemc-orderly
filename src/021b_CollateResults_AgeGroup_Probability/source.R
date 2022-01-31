#  code common amongst `aggregationCode` sheets

# function to recursively create directories if missing (also use in plots)
create_dirs_r <- function(dir_path) {

  # split by "/"
  dirs <- stringr::str_split(dir_path, "/")[[1]]

  # check if dir_path is for dir (required) or specific file
  # does our string end in "/"? Then likely a dir
  cond <- substr(dir_path, nchar(dir_path), nchar(dir_path))
  cond <- grepl("/", cond)
  # does last word contain "."? Likely a file
  cond <- cond & !grepl(".", last(dirs))
  if (!cond) {
    dirs <- dirs[-length(dirs)] # remove file name
  }
  if (length(dirs > 0)) {
    # loop through each directory level in target directory, create any missing
    for (i in seq_along(dirs)) {
      # "recursively" check directories
      if (i == 1) {
        spec_dir <- dirs[i]
      } else spec_dir <- paste(dirs[1:i], collapse = "/")
      # create if missing
      if (!dir.exists(spec_dir)) {
        dir.create(spec_dir)
      }
    }
  }
}


# pull samples for various kinds of circumcision from TMB posterior fit
# and join with area data from "results"
prepare_sample_data <- function(N = 100,
                                # area_lev, # no need for areas here
                                # areas_wide,
                                populations,
                                no_prog_results = NULL,
                                prog_results = NULL,
                                no_prog_tmb_fit,
                                prog_tmb_fit,
                                type
) {

  if (is.null(no_prog_results) & is.null(prog_results)) {
    stop("cannot have prog_results == no_prog_results == NULL")
  }

  #########################################
  ### Loading rates from survival model ###
  #########################################

  if (!type %in% c("probability", "incidence", "prevalence") |
      length(type) > 1) {
    stop("Please choose a valid type
         (one of 'probability', 'incidence', 'prevalence'")
  }

  # append samples from different circumcision models together (and pops)
  append_fun <- function(tmp, fit, populations, type) {

    # different objects to pull samples from fit, based on desired aggregation
    if (type == "probability") {
      mmc <- "haz_mmc"
      tmc <- "haz_tmc"
    } else if (type == "incidence") {
      mmc <- "inc_mmc"
      tmc <- "inc_tmc"
      mmct <- "inc_mmct"
    } else if (type == "prevalence") {
      mmc <- "cum_inc_mmc"
      tmc <- "cum_inc_tmc"
      mmct <- "cum_inc_mmct"
    }

    # word to be pasted onto the end of circ type below
    if (type == "prevalence") {
      category = "coverage"
    } else category = type

    tmpx_1 <- tmpx_2 <- tmpx_3 <- tmpx_4 <- tmpx_5 <- tmpx_6 <- tmp
    if (type == "incidence") {
      tmpx_7 <- tmpx_8 <- tmpx_9 <- tmpx_10 <- tmpx_11 <- tmpx_12 <- tmp
    } else {
      tmpx_7 <- tmpx_8 <- tmpx_9 <- tmpx_10 <- tmpx_11 <- tmpx_12 <- NULL
    }

    #
    tmpx_1[, paste0("samp_", 1:N)] <- fit$sample[[mmc]][, 1:N]
    tmpx_1$type <- paste("MMC-nT", category)
    if (tmp$model[1] == "No program data") {

      tmpx_2[, paste0("samp_", 1:N)] <- 0
      tmpx_3[, paste0("samp_", 1:N)] <- fit$sample[[tmc]][, 1:N]
      tmpx_4[, paste0("samp_", 1:N)] <- fit$sample[[mmc]][, 1:N]
      tmpx_5[, paste0('samp_', 1:N)] <- fit$sample[[tmc]][, 1:N]
      tmpx_6[, paste0("samp_", 1:N)] <- fit$sample[[tmc]][, 1:N] +
        fit$sample[[mmc]][, 1:N]

    } else if (tmp$model[1] == "With program data") {
      if (type == "probability") {

        tmpx_2[, paste0("samp_", 1:N)] <- fit$sample$probs[, 1:N] *
          fit$sample[[tmc]][, 1:N]
        tmpx_3[, paste0("samp_", 1:N)] <- (1 - fit$sample$probs[, 1:N]) *
          fit$sample[[tmc]][ ,1:N]
        tmpx_4[, paste0("samp_", 1:N)] <- fit$sample[[mmc]][, 1:N] +
          fit$sample$probs[, 1:N] * fit$sample[[tmc]][, 1:N]
        tmpx_5[, paste0('samp_', 1:N)] <- fit$sample[[tmc]][, 1:N]
        tmpx_6[, paste0("samp_", 1:N)] <- fit$sample[[tmc]][, 1:N] +
          fit$sample[[mmc]][, 1:N]
      } else {

        tmpx_2[, paste0("samp_", 1:N)] <- fit$sample[[mmct]][,1:N]
        tmpx_3[, paste0("samp_", 1:N)] <- fit$sample[[tmc]][,1:N]
        tmpx_4[, paste0("samp_", 1:N)] <- fit$sample[[mmc]][,1:N] +
          fit$sample[[mmct]][,1:N]
        tmpx_5[, paste0("samp_", 1:N)] <- fit$sample[[tmc]][,1:N] +
          fit$sample[[mmct]][,1:N]
        tmpx_6[, paste0("samp_", 1:N)] <- fit$sample[[mmc]][,1:N] +
          fit$sample[[tmc]][,1:N] + fit$sample[[mmct]][,1:N]
      }
    }
    tmpx_2$type <- paste("MMC-T", category)
    tmpx_3$type <- paste("TMC", category)
    tmpx_4$type <- paste("MMC", category)
    tmpx_5$type <- paste("TMIC", category)
    tmpx_6$type <- paste("MC", category)

    # Samples for the number of MCs performed (for incidence)
    if (type == "incidence") {
      tmpx_7 <- tmpx_1;  tmpx_7$type <- 'MMC-nTs performed'
      tmpx_8 <- tmpx_2;  tmpx_8$type <- 'MMC-Ts performed'
      tmpx_9 <- tmpx_3;  tmpx_9$type <- 'TMCs performed'
      tmpx_10 <- tmpx_4; tmpx_10$type <- 'MMCs performed'
      tmpx_11 <- tmpx_5; tmpx_11$type <- 'TMICs performed'
      tmpx_12 <- tmpx_6; tmpx_12$type <- 'MCs performed'
    }

    # Appending things together
    tmp <- as.list(mget(paste0("tmpx_", 1:12))) %>%
      bind_rows() %>%
      # only keep relevant columns
      select(area_id, area_name, year, age, type, model, contains("samp_")) %>%
      # join in region populations
      left_join(
        # only keep relevant columns in populations
        (populations %>%
           select(
             all_of(names(tmp)[names(tmp) %in% names(populations)]),
             population
           ))
      ) %>%
      relocate(population, .before = samp_1)

    # filter out na populations, with an appropriate message
    if (any(is.na(tmp$population)) == TRUE) {
        n1 <- nrow(tmp)
        tmp <- filter(tmp, !is.na(population))
        n2 <- nrow(tmp)
        if (n2 == 0) stop("No populations present in data")
        message(paste0("Missing population for ", n1 - n2, " records"))
    }


    return(tmp)
  }

  # Model with Probability of MC with no program data (only surveys)
  if (!is.null(no_prog_results)) {
    tmp1 <- no_prog_results %>%
      mutate(model = "No program data")
    tmp1 <- append_fun(tmp1, no_prog_tmb_fit, populations, type = type)
  } else {
    tmp1 <- NULL
  }

  # Model with Probability of MC with both programme and survey data
  if (!is.null(prog_results)) {
    # tmp2 <- ('Output/Predictions/Results_DistrictAgeTime_ByType_withProgram_withBorrowing.csv')
    tmp2 <- prog_results %>%
      mutate(model = "With program data")
    tmp2 <- append_fun(tmp2, prog_tmb_fit, populations, type = type)
  } else {
    tmp2 <- NULL
  }

  # Appending things together
  results <- rbind(tmp1, tmp2)

  return(results)
}


#' change area ids from one hierarchy level to another
#' (works going "down" (i.e. less granular), but does vice versa work?)
add_area_id <- function(df, df_areas_wide, par, add_keep_cols = NULL) {

    # Getting area_id's
    area_lev_current_id = paste0("area_id", par$area_lev)
    # The level we want
    area_lev_select_id = paste0("area_id", par$area_lev_select)
    area_lev_select_name = paste0("area_name", par$area_lev_select)

    #' only select columns in our dataframe ("model" may be missing,
    #' and `age` and `age_group` are interchangable (can definitely improve this!!)
    select_cols <- c("year", "age", "age_group", "population", "type", "model")
    select_cols <- select_cols[select_cols %in% names(df)]
    # additional columns to keep, if supplied
    if (!is.null(add_keep_cols)) {
        select_cols <- unique(c(select_cols, add_keep_cols))
        # interfers with selection below
        select_cols <- select_cols[!select_cols %in% c("area_id", "area_name")]
    }

    df_area_id <- df %>%
        # join in area names for chosen area_id
        left_join(df_areas_wide %>%
                      dplyr::select(
                          area_id = all_of(area_lev_current_id), # current level
                          all_of(area_lev_select_id), # desired level and
                          area_name = all_of(area_lev_select_name) # corresponding name
                      ) %>%
                      distinct(),
                  by = c("area_id")) %>%
        # Select the right columns (account for case when we are at the lowest level)
        dplyr::select(
            "area_id" = ifelse(par$area_lev_select == par$area_lev,
                               "area_id",
                               area_lev_select_id),
            "area_name",
            all_of(select_cols),
            all_of(par$sample_cols)
        )
    return(df_area_id)
}

# collect results for lower area hierarchies by joining higher area
# hierarchies (should really allow inputs to add_keep_cols here!)
combine_areas <- function(.data, area_lev, join) {

    # all area levels in the data (0 indexed)
    area_levs <- seq_len(area_lev) - 1

    # collect results for lower area hierarchies by joining higher area
    # hierarchies (do so until you reach "area 0")
    if (area_levs[1] > -1) {
        results_list <- lapply(area_levs, function(x) {
            add_area_id(
                df = (.data %>% select(-area_name)),
                df_areas_wide = areas_wide,
                par = list("area_lev" = area_lev,
                           "area_lev_select" = x),
                add_keep_cols = names(.data)[names(.data) %like% "samp_"]
            )}
        )
        # also add highest area hierarchy data
        results_list[[length(results_list) + 1]] <- results
    } else {
        results_list <- results
    }

    # return list or dataframe?
    if (join == TRUE) {
        return(
            as.data.frame(data.table::rbindlist(results_list, use.names = T))
        )
    } else {
        return(results_list)
    }
}


# function to increase area levels (should work for decreasing as well??)
increment_survey_area <- function(survey_data,
                                  areas_wide,
                                  par) {

    # store to later keep only these
    orig_names <- names(survey_data)

    # take only those areas in the area level you want increased:
    survey_data_area_lev <- survey_data %>%
        mutate(area_level = as.numeric(substr(area_id, 5, 5))) %>%
        filter(area_level == !!par$area_lev)
    survey_data <- survey_data %>%
        anti_join(survey_data_area_lev)

    # change area level to desired level
    # (should expand this to use combine_areas from aggregations!)
    survey_data_area_lev <- add_area_id(
        df = (survey_data_area_lev),
        df_areas_wide = areas_wide,
        par = par,
        add_keep_cols = names(survey_data_area_lev)
    ) %>%
        select(all_of(orig_names))

    # join back with other surveys
    survey_data <- rbind(survey_data_area_lev, survey_data) # %>%
    # rename(geoloc_area_id = area_id)
}

# aggregate by area, year, age and type (weighted by population),
# and then convert to a percentage/probability
aggregate_sample <- function(.data, aggr_cols = NULL) {

    # columns to aggregate by (only take those in our data)
    if (is.null(aggr_cols)) {
        aggr_cols <- c("area_id", "area_name", "year",
                       "age", "age_group", "model", "type")
        aggr_cols <- aggr_cols[aggr_cols %in% names(.data)]
    }

    #   .data <- .data %>%
    #     # Multiplying by population to population weight
    #     mutate(across(contains("samp_"), ~ . * population)) %>%
    #     # Getting summarising samples
    #     group_by(across(all_of(aggr_cols))) %>%
    #     # summarise_all(sum) %>%
    #     summarise(across(everything(), sum), .groups = "drop") %>%
    #     # Dividing by population to population weight
    #     mutate(across(contains("samp_"), ~ . / population))

    .data <- .data %>%
        mutate(across(contains("samp_"), ~ . * population))

    # dplyr summarise:
    # .data <- .data %>%
    #    group_by(across(all_of(aggr_cols))) %>%
    #    summarise_all(sum)

    # data.table solution
    .data <- setDT(.data)[,
                     lapply(.SD, sum, na.rm = T),
                     by = c(aggr_cols),
                     .SDcols = c("population", paste0("samp_", c(1 : 100)))
             ]
    # gc()
    .data <- as.data.frame(.data) %>%
        mutate(across(contains("samp_"), ~ . / population))

    return(.data)
}

# aggregate by area, year and type, for each individual age group modelled
# (weighted by population), and then convert to a percentage/probability
aggregate_sample_age_group <- function(
    results_list,
    add_groups,
    remove_groups) {

    if(inherits(results_list, "data.frame")) {
        stop("requires list from combine_areas (set argument join = FALSE)")
    }

    # standard age groups
    age_groups <- c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29',
                    '30-34', '35-39', '40-44', '45-49', '50-54', '54-59',
                    '0+',    '10+',   '15+',   '15-24', '10-24',
                    '15-29', '10-29', '15-39', '10-39', '15-49', '10-49')
    # add or remove groups as desired
    if (!missing(add_groups)) age_groups <- c(age_groups, add_groups)
    if (!missing(remove_groups)) {
        age_groups <- age_groups[-(age_groups %in% remove_groups)]
    }

    # Multiplying by population to population weight
    results_list <- lapply(results_list, function(x) {
        x %>%
            mutate(across(contains("samp_"), ~ . * population))
    })
    # aggregate sample for each age group
    results <- lapply(seq_along(age_groups), function(i) {
        # If upper limit use this split
        if (grepl('-', age_groups[i]) == TRUE) {
            age1 <- as.numeric(strsplit(age_groups[i], '-')[[1]][1])
            age2 <- as.numeric(strsplit(age_groups[i], '-')[[1]][2])
        }
        # If no upper limit use this split
        if (grepl('\\+', age_groups[i]) == TRUE) {
            age1 <-  as.numeric(strsplit(age_groups[i], '\\+')[[1]][1])
            age2 <- Inf
        }
        results_list_loop <- lapply(results_list, function(x) {
            x <- x %>%
                # take results for age group i
                filter(age >= age1, age <= age2) %>%
                select(-age)
            # Getting summarising samples
            # group_by(area_id, area_name, year, model, type) %>%
            # summarise(across(everything(), sum), .groups = "drop") %>%

            x <- setDT(x)
            x <- x[,
                   lapply(.SD, sum, na.rm = T),
                   by = c("area_id", "area_name", "year", "model", "type"),
                   .SDcols = c("population", paste0("samp_", c(1 : N)))
            ]
            x <- x %>%
                # Adding age group
                mutate(age_group = age_groups[i])
        })
        # Printing index
        print(age_groups[i])
        # return ages
        return(results_list_loop)
        # Appending together
        # results <- rbind(results, rbindlist(results_list_loop))
    })
    # join together
    results <- as.data.frame(rbindlist(lapply(results, rbindlist)))

    # Multiplying by population to population weight
    # (don't do this for "N performed", if present)
    results <- results %>%
        mutate(across(contains("samp_"), ~ ifelse(grepl("performed", type),
                                                  .,
                                                  . / population))
        )


    return(results)
}

# Getting summary statistics
posterior_summary_fun <- function(.data, N = 100) {

    # ensure numeric columns are after categorical
    .data <- .data %>%
        relocate(population | contains("samp_"), .after = everything())

    # pull locations of columns to "group by"
    id_cols <- seq_along(names(.data)[!names(.data) %like% "samp"])

    # use data.table as this can be quite slow for larger countries
    if(!inherits(.data, "data.table")) .data <- setDT(.data)

    # pivot to calculate row means and sds of samples for each stratification
    .data_long <- melt(.data, id.vars = id_cols,
                       measure.vars = c(paste0("samp_", 1:100)))
    .data <- .data_long[,
               '.'
               (mean = mean(value, na.rm = TRUE),
                   sd = sd(value, na.rm = TRUE)),
               keyby = c(names(.data)[id_cols])] # group by all categories]

    # calculate median and CI
    quantiles <- .data_long[, {
        quantiles = quantile(value, c(0.5, 0.025, 0.975), na.rm = TRUE, names = FALSE)
        ':='
        list(
            median = quantiles[1],
            lower = quantiles[2],
            upper = quantiles[3]
        )
    }, keyby = c(names(.data)[id_cols]), ]

    .data <- as.data.frame(merge(.data, quantiles))
}

# function to get change in prevalence/coverage from a given year
prevalence_change <- function(results, spec_year) {
    # pull samples from coverage in chosen year
    spec_year_results <- results %>%
        filter(year == spec_year) %>%
        select(-c(year, population)) %>%
        tidyr::pivot_longer(contains("samp_"), values_to = "prev_value")

    # join into results_change_2008 for corresponding categorical variables and subtract
    results_change_year <- results %>%
        tidyr::pivot_longer(contains("samp_")) %>%
        left_join(spec_year_results) %>%
        mutate(value = value - prev_value) %>%
        select(-prev_value) %>%
        tidyr::pivot_wider(., names_from = name, values_from = value) %>%
        mutate(type = paste0("Change in ", type, " from 2008"))
}

# calculate number of people circumcised (as well as unmet need)
n_circumcised <- function(results) {
    # Getting number of circumcised men
    tmp <- split(results, results$type)

    # get circumcised population by type
    tmp <- lapply(tmp, function(x) {
        x %>%
            mutate(
                across(contains("samp_"), ~ . * population),
                type = paste0("Number circumcised (",
                              stringr::str_remove(type, " coverage"),
                              ")")
            )
    })
    # also calculate unmet need
    tmp[[length(tmp) + 1]] <- results %>%
        filter(type == "MC coverage") %>%
        mutate(
            across(contains("samp_"), ~ population * (1 - .)),
            type = "Unmet need"
        )

    # Append together
    results_n <- as.data.frame(data.table::rbindlist(tmp, use.names = T))
}

# Merge regional information on the dataset (i.e. parent area info)
merge_area_info <- function(results, areas) {

    # Merging regional information on the dataset (i.e. parent area info)
    results <- results %>%
        # Adding region information
        left_join(
            (areas %>%
                 select(area_id:area_level)),
            by = c("area_id", "area_name")
        ) %>%
        relocate("area_level", .after = "area_name") %>%
        left_join(
            (areas %>%
                 select(parent_area_id = area_id,
                        parent_area_name = area_name)),
            by = "parent_area_id"
        ) %>%
        relocate(contains("area"))
}
