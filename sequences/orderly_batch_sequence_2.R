#### Orderly Batch Run on Pocatello ####
iso3 <- c(
  "LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
  "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
  "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
  "ZAF", "LBR", "GHA", "GMB", "NGA", "COD", "BWA", "CAF", "GNB", "GNQ"
)

# iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE")
# iso3 <- c("ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV")
# iso3 <- c("GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH") 
# iso3 <- c("ZAF", "LBR", "GHA", "GMB", "NGA", "COD", "BWA", "CAF", "GNB", "GNQ") 

iso3_splits <- split(seq_len(length(iso3)), sort(seq_len(length(iso3)) %% 6))
iso3 <- iso3[iso3_splits[[6]]][5:6]

# task <- "val_02a_investigating_var_corr"
# task <- "01b_empirical_rates"
# task <- "val_03a_ppc_prevalence"
# task <- "val_hyperpar_temporal_prior_investigation"
# task <- "01a2_model_aggregate_spec_model"
task <- "val_fit_model_pooled_hyperpars"
# task <- "val_00a_oos_validation"

orderly_root <- here::here()

# check_task <- "val_00a_oos_validation"
# check_task <- "01_modelling"
# check_task <- "val_hyperpar_temporal_prior_investigation"
# check_task <- "01a2_model_aggregate_spec_model"
check_task <- task

# search for orderly tasks already performed
(names <- sapply(iso3, function(x) {
  orderly::orderly_search(
    name = check_task,
    query = "latest(parameter:cntry == cntry)",
    # query = "latest(parameter:cntry == cntry && parameter:area_lev == area_lev)",
    # query = "latest(parameter:cntry == cntry && parameter:mod == mod)",
    parameters = list(cntry = as.character(x)),
    # parameters = list(cntry = as.character(x), area_lev = 0),
    # parameters = list(
    #   cntry = as.character(x), 
    #   mod   = "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC2"
    # ),
    draft = TRUE,
    root = orderly_root
  )
}))

if (inherits(names, "list")) {
  names <- sapply(names, `[`, 1)
}

# only run reports that haven't already been run
# iso3 <- iso3[is.na(names)]

pars <- data.frame("cntry" = iso3)
# mods <- c(
#   "Surv_SpaceAgeTime_ByType_withUnknownType",
#   "Surv_SpaceAgeTime_ByType_withUnknownType2", 
#   "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC2"
# )
# pars <- tidyr::crossing("cntry" = iso3, "mod" = mods)

priors <- c("AR 1", "RW 1", "RW 2")
pars <- tidyr::crossing("cntry" = iso3, "prior" = priors)

(names <- sapply(seq_len(nrow(pars)), function(i) {
  orderly::orderly_search(
    name = check_task,
    # query = "latest(parameter:cntry == cntry)",
    # query = "latest(parameter:cntry == cntry && parameter:mod == mod)",
    query = "latest(parameter:cntry == cntry && parameter:prior == prior)",
    parameters = list(
      cntry = as.character(pars$cntry[i]), 
      prior = as.character(pars$prior[i])
    ),
    draft = TRUE,
    root = orderly_root
  )
}))

pars <- pars[is.na(names), ]



# Fit for MWI and UGA with most recent PHIA surveys removed and added
# pars$is_paper <- FALSE

# pars$mod <- "Surv_SpaceAgeTime_ByType_withUnknownType"
# pars$mod <- "Surv_SpaceAgeTime_ByType_withUnknownType2"
# pars$mod <- "Surv_SpaceAgeTime_ByType_withUnknownType_Const_Paed_MMC2"

# pars$prior <- "RW 1"
# pars$prior <- "RW 2"

# pars$area_lev <- 0

possibly_batch <- purrr::possibly(orderly::orderly_batch, otherwise = NA)

# run fixed variance & covariance tasks 
ids <- orderly::orderly_batch(
  name = task, 
  parameters = pars,
  root = orderly_root,
  continue_on_error = TRUE
)
