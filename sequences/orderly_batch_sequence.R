#### Orderly Batch Run on Pocatello ####
iso3 <- c(
  "LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
  "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
  "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
  "ZAF", "LBR", "GHA", "GMB", "NGA", "COD"
   # "BWA", "CAF", "GNB", "GNQ", "LSO"
)

# iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE")
# iso3 <- c("ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV")
# iso3 <- c("GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH")
# iso3 <- c("ZAF", "LBR", "GHA", "GMB", "NGA", "COD", "BWA", "CAF", "GNB", "GNQ")
# iso3 <- c("LBR", "SEN", "NER", "GIN", "COD") # run for countries with no type

# iso3_splits <- split(seq_len(length(iso3)), sort(seq_len(length(iso3)) %% 6))
# iso3 <- iso3[iso3_splits[[6]]]
# use last iso3 for each group
# iso3 <- vapply(seq_along(iso3_splits), function(i) {
#   dplyr::last(iso3[iso3_splits[[i]]])
# }, character(1))

# iso3 <- "MWI"

# task <- "val_02a_investigating_var_corr"
# task <- "01b_empirical_rates"
# task <- "val_03a_ppc_prevalence"
# task <- "val_hyperpar_temporal_prior_investigation2"
task <- "01a3_model_aggregate_spec_model_RW"

orderly_root <- here::here()

# check_task <- "val_00a_oos_validation"
# check_task <- "01_modelling"
# check_task <- "val_hyperpar_temporal_prior_investigation2"
check_task <- task

# search for orderly tasks already performed
(names <- sapply(iso3, function(x) {
  orderly::orderly_search(
    name = check_task,
    # query = "latest(parameter:cntry == cntry)",
    query = "latest(parameter:cntry == cntry && parameter:rw_order == rw_order)",
    # parameters = list(cntry = as.character(x)),
    parameters = list(cntry = as.character(x), rw_order = 2),
    # draft = TRUE,
    root = orderly_root
  )
}))
if (inherits(names, "list")) {
  names <- unlist(names)
}

# only run reports that haven't already been run
# iso3 <- iso3[!iso3 %in% names(names)]
# iso3 <- iso3[!is.na(names)]
iso3 <- iso3[is.na(names)]
# iso3 <- iso3[iso3 %in% names(names[is.na(names)])]

# # split these into 6 parts
# iso3_splits <- split(seq_len(length(iso3)), sort(seq_len(length(iso3)) %% 6))
# iso3_splits <- split(seq_len(length(iso3)), sort(seq_len(length(iso3)) %% 3))
# iso3 <- rev(iso3[iso3_splits[[3]]])
# iso3 <- vapply(seq_along(iso3_splits), function(i) {
  # dplyr::last(iso3[iso3_splits[[i]]])
# }, character(1))

pars <- data.frame("cntry" = iso3)
# Fit for MWI and UGA with most recent PHIA surveys removed and added
# pars$is_paper <- TRUE
# pars <- rbind(
#     pars,
#     data.frame("cntry" = c("MWI", "UGA"), "is_paper" = FALSE)
# )
# OR
# pars$is_paper <- FALSE

pars <- dplyr::bind_rows(
  dplyr::mutate(pars, rw_order = 1),
  dplyr::mutate(pars, rw_order = 2)
)

possibly_batch <- purrr::possibly(orderly::orderly_batch, otherwise = NA)

# run fixed variance & covariance tasks 
ids <- orderly::orderly_batch(
  name = task,
  parameters = pars,
  root = orderly_root,
  continue_on_error = TRUE
)
