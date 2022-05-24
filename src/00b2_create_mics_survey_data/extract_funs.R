dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}

dhscc_to_iso3 <- function(dhscc) {
  dhsc <- rdhs::dhs_countries()
  dictionary <- setNames(dhsc$ISO3_CountryCode, dhsc$DHS_CountryCode)
  val <- dplyr::recode(dhscc, !!!dictionary)

  if (any(!val %in% dictionary)) {
    stop("DHS Country Code not found: ", paste(val[!val %in% dictionary], collapse = ", "))
  }

  val
}

#' Recode ID variables in MICS surveys
#'
#' @param variable_recode Variable recode book
#' @param survey_id_c Survey ID
#' @param dataset_type Dataset type e.g.....
#' @description MICS surveys do not have consistent cluster, household, and line number variables. This standardises them
#'
#' @export

recode_mics_id_vars <- function(variable_recode, survey_id_c, dataset_type) {

  # Finds if there are custom variable names associated with this survey ID and dataset type
  custom_recode <- filter(variable_recode, survey_id == survey_id_c, dataset == dataset_type)

  # Finds the default variable names for MICS surveys and the specified dataset type
  default_recode <- filter(
    variable_recode, survey_id == "_default_mics",
    dataset == dataset_type
  )

  # variable_df <- data.frame(variable = c("cluster_id", "household", "line"))
  variable_df <- data.frame(variable = c("cluster_id", "household", "line", "mics_area_name", "mics_area_level"))

  variable_df <- variable_df %>%
    left_join(custom_recode, by = "variable")

  # Combines the custom and default variable name dataframes, using the defaults in the absence of custom variables
  variable_df <- variable_df %>%
    filter(!is.na(var_raw)) %>%
    bind_rows(variable_df %>%
      filter(is.na(var_raw)) %>%
      select(variable) %>%
      left_join(default_recode, by = "variable")) %>%
    filter(!is.na(var_raw))

  id_vars <- filter(variable_df, dataset == dataset_type)$var_raw
  names(id_vars) <- filter(variable_df, dataset == dataset_type)$variable

  id_vars
}

#' Extract survey variables
#'
#' @param df Raw survey dataframe
#' @param survey_id_c Survey ID
#' @param variable_recode Variable recode book
#' @param dataset_type Dataset type e.g.....
#' @param analysis One of "circ", "sexbehav" etc
#' @param add_vars Additional variables in raw surveys that are desirable as is
#' (i.e without recoding).
#'
#' @export

# df <- circ_raw[[1]]
# variable_recode_test <- variable_recode
# variable_recode <- list(variable_recode)
# survey_id_c, variable_recode, dataset_type, analysis_c

extract_survey_vars <- function(df, survey_id_c, variable_recode, dataset_type, analysis_c, add_vars = NULL) {
  message(survey_id_c)

  surv_type <- substr(survey_id_c, 8, stringr::str_length(survey_id_c))

  ## If individual ID is to be used as primary key - this if statement needs editing
  if (surv_type %in% c("DHS", "AIS", "MIS")) {
    if (dataset_type == "ir") {
      id_vars <- c(cluster_id = "v001", household = "v002", line = "v003")
    } else if (dataset_type == "mr") {
      if (survey_id_c == "TZA2012AIS") {
        id_vars <- c(cluster_id = "v001", household = "v002", line = "v003")
      } else {
        id_vars <- c(cluster_id = "mv001", household = "mv002", line = "mv003")
      }
    } else {
      stop("Dataset type not recognised")
    }

    if (surv_type == "AIS") {
      if (analysis_c == "circ" & dataset_type == "ir") {
        df <- filter(df, aidsex == 1)
      }
      surv_type <- "DHS"
    }
  } else if (surv_type == "PHIA") {
    id_vars <- c(cluster_id = "centroidid", household = "householdid", line = "personid")
  } else if (surv_type == "MICS") {
    id_vars <- recode_mics_id_vars(variable_recode, survey_id_c, dataset_type)

    colnames(df) <- tolower(colnames(df))
  } else {
    stop("Survey type not recognised")
  }

  # Finds if there are custom variable names associated with this survey ID and dataset type
  custom_recode <- filter(variable_recode, survey_id == survey_id_c, dataset == dataset_type)

  # Finds the default variable names for specified survey and dataset types
  default_recode <- filter(
    variable_recode,
    dataset == dataset_type,
    survey_id == paste0("_default_", tolower(surv_type)),
    !variable %in% c("cluster_id", "household", "line")
  )
  # !variable %in% c("cluster_id", "household", "line", "mnweight"))

  variable_df <- variable_recode %>%
    filter(
      analysis == analysis_c,
      str_detect(survey_id, "_default")
    ) %>%
    distinct(variable) %>%
    left_join(custom_recode, by = "variable")

  # Combines the custom and default variable name dataframes, using the defaults in the absence of custom variables
  variable_df <- variable_df %>%
    filter(!is.na(var_raw)) %>%
    bind_rows(variable_df %>%
      filter(is.na(var_raw)) %>%
      select(variable) %>%
      left_join(default_recode, by = "variable")) %>%
    filter(!is.na(var_raw))

  opt_var <- filter(variable_df)$var_raw %>% setNames(filter(variable_df)$variable)

  # Mandatory variables are cluster_id, household, line number (to be replaced with individual ID).
  # Any "analysis variables" are optional.
  if (!is.null(add_vars)) {
    id_vars <- c(id_vars, add_vars)
  }

  df <- df %>%
    select(
      all_of(id_vars)[id_vars %in% names(df)],
      any_of(opt_var)
    )
}

#' Recode survey variables
#'
#' @param col Column passed from across()
#' @param col_name Column name passed by dplyr::cur_column()
#' @param survey_id_c Survey ID
#' @param dataset_type Dataset type e.g.....
#' @param analysis One of "circ", "sexbehav" etc
#'
#' @export


val_recode <- function(col, col_name, survey_id_c, dataset_type, analysis_c) {
  value_recode <- value_recode %>%
    filter(variable == col_name)

  # Because all values in the value book come from one excel column that includes strings, the NAs are read as _NA_character which screws up the recode later.
  # If, for a given variable, the values are only 0-9 and NA, force to be numeric.
  if (!length(value_recode$value[!value_recode$value %in% c(0:9, NA)])) {
    value_recode$value <- as.numeric(value_recode$value)
  }

  surv_type <- substr(survey_id_c, 8, stringr::str_length(survey_id_c))

  # Required because the AIS default variables are from _default_dhs
  if (surv_type == "AIS") {
    surv_type <- "DHS"
  }

  # Same logic applies as with extracting variables using custom and defaults.
  custom_recode <- filter(
    value_recode,
    dataset == dataset_type,
    survey_id == survey_id_c
  )

  default_recode <- filter(
    value_recode,
    dataset == dataset_type,
    analysis == analysis_c,
    survey_id == paste0("_default_", tolower(surv_type))
  )

  value_df <- default_recode %>%
    distinct(variable) %>%
    left_join(custom_recode, by = "variable")

  value_recode <- value_df %>%
    filter(!is.na(survey_id)) %>%
    bind_rows(value_df %>%
      filter(is.na(survey_id)) %>%
      select(variable) %>%
      left_join(default_recode, by = "variable")) %>%
    filter(variable == col_name) %>%
    droplevels()

  if (nrow(value_recode) == 0) {
    value_recode <- custom_recode
  }

  vec <- value_recode$value
  names(vec) <- value_recode$val_raw

  recode(col, !!!vec)
}

recode_survey_variables <- function(df, survey_id_c, value_recode, dataset_type, analysis_c) {
  message(survey_id_c)

  surv_type <- substr(survey_id_c, 8, stringr::str_length(survey_id_c))
  if (surv_type == "AIS") surv_type <- "DHS"

  recode_columns <- unique(
    filter(
      value_recode,
      survey_id == paste0("_default_", tolower(surv_type)),
      dataset == dataset_type,
      analysis == analysis_c
    )$variable
      %>% as.character()
  )

  survey_specific_columns <- unique(filter(
    value_recode,
    survey_id == survey_id_c,
    dataset == dataset_type,
    analysis == analysis_c
  )$variable)

  recode_columns <- unique(c(recode_columns, survey_specific_columns))

  if ("area_name" %in% names(df)) {
    spec_areas <- df$area_name
    df <- select(df, -area_name)
  } else {
    spec_areas <- NULL
  }

  df <- df %>%
    mutate(across(everything(), as.numeric),
      across(any_of(recode_columns), ~ val_recode(.x, cur_column(), survey_id_c, dataset_type, analysis_c)),
      survey_id = survey_id_c,
      individual_id = dhs_individual_id(cluster_id, household, line)
    ) %>%
    type.convert() %>%
    select(survey_id, individual_id, everything())

  if (!is.null(spec_areas)) df <- mutate(df, area_name = spec_areas)

  return(df)
}

# function to change Ethiopian to Gregorian Calendar (date must be CMC)
ethiopian_to_greg <- function(cmc = NULL) {
  ey <- trunc((cmc - 1) / 12) + 1900
  em <- cmc - (ey - 1900) * 12
  ed <- 1

  joffset <- 1723856
  n <- 30 * (em - 1) + ed - 1 # ed - 1 if actually 0
  jd <- joffset + 365 + 365 * (ey - 1) + trunc(ey / 4) + n

  z <- jd + 0.5
  w <- trunc((z - 1867216.25) / 36524.25)
  x <- trunc(w / 4)
  a <- z + 1 + w - x
  b <- a + 1524
  c <- trunc((b - 122.1) / 365.25)
  d <- trunc(365.25 * c)
  e <- trunc((b - d) / 30.6001)
  f <- trunc(30.6001 * e)
  day <- b - d - f

  month <- e - 1

  month.low <- (month <= 12) * (e - 1)
  month.high <- (month > 12) * (e - 13)
  month <- month.low + month.high

  year.low <- (month < 3) * (c - 4715)
  year.high <- (month >= 3) * (c - 4716)
  year <- year.low + year.high

  outcmc <- 12 * (year - 1900) + month
  return(outcmc)
}

# convert cmc date
cmc_as_date <- function(x, selectday, format.out) {
  
  if (is.logical(x)) {
    return(x)
  } # all values are NA
  if (!is.numeric(x)) {
    print("WARNING in cmc.as.Date: x is not numeric")
    if (is.character(x)) x <- as.numeric(x) else print("ERROR in cmc_as_Date: x not numeric and not character.")
  }

  if (missing(selectday)) selectday <- 1
  if (missing(format.out)) format.out <- "%Y-%m-%d"
  cmc <- x
  # cmc[cmc<=0] <- NA
  # base <- 1900
  base <- 1000
  cmc <- 900 * 12 + x
  year <- base + trunc((cmc - 1) / 12)
  month <- cmc - (year - base) * 12
  day <- rep(selectday, length(cmc))
  year
  month
  z <- ifelse(is.na(cmc), NA, paste(year, "-", month, "-", day, sep = ""))
  dd <- as.Date(z, format = "%Y-%m-%d")
  date <- format(dd, format.out)
  return(date) # format produces a character vector
}
