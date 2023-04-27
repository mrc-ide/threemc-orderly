#### Orderly Batch Run on Pocatello ####

#### Libs and Metadata ####

library(dplyr)
library(tidyr)

iso3 <- c(
  "LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
  "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
  "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
  "ZAF", "LBR", "GHA", "GMB", "NGA", "COD"
   # "BWA", "CAF", "GNB", "GNQ", "LSO"
)

# iso3 <- "ZMB"
# iso3 <- iso3[!iso3 %in% c("LSO", "ZWE")]

task <- "val_hyperpar_temporal_prior_investigation3"

orderly_root <- here::here()

#### Create parameters data.frame ####

# AR hyperpars
ar_pars <- data.frame(
  "rw_order"               = 0,
  "logsigma_time_mmc"      = seq(0, 2.5, by = 0.25), 
  "logsigma_agetime_mmc"   = seq(0, 2.5, by = 0.25), 
  "logsigma_spacetime_mmc" = seq(-1.5, 1, by = 0.25)
)
# RW 1 hyperpars
rw_1_pars <- data.frame(
  "rw_order"               = 1,
  "logsigma_time_mmc"      = seq(-3, 1, by = 0.25),
  "logsigma_agetime_mmc"   = seq(-3, 1, by = 0.25),
  "logsigma_spacetime_mmc" = seq(-3, 1, by = 0.25)
)
# RW 2 hyperpars
rw_2_pars <- data.frame(
  "rw_order"               = 2,
  "logsigma_time_mmc"      = seq(-2, 1, by = 0.25),
  "logsigma_agetime_mmc"   = seq(-3, 0, by = 0.25),
  "logsigma_spacetime_mmc" = seq(-6, -3, by = 0.25)
)
pars <- bind_rows(
  ar_pars,
  rw_1_pars,
  rw_2_pars
)
pars <- crossing(pars, "cntry" = iso3) %>%
  arrange(cntry)

# par_df_splits <- split(seq_len(nrow(pars)), sort(seq_len(nrow(pars)) %% 6))
# pars <- pars[par_df_splits[[6]], ]


#### Check for tasks having already been run ####

check_task <- task
# # search for orderly tasks already performed
# names <- sapply(iso3, function(x) {
names <- lapply(seq_len(nrow(pars)), function(i) {
 orderly::orderly_search(
   name = check_task,
   query = "latest(parameter:cntry == cntry && parameter:rw_order == rw_order && parameter:logsigma_time_mmc == logsigma_time_mmc && parameter:logsigma_agetime_mmc == logsigma_agetime_mmc && parameter:logsigma_spacetime_mmc == logsigma_spacetime_mmc)",
   parameters = list(cntry = pars$cntry[i], rw_order = pars$rw_order[i], logsigma_time_mmc = pars$logsigma_time_mmc[i], logsigma_agetime_mmc = pars$logsigma_agetime_mmc[i], logsigma_spacetime_mmc = pars$logsigma_spacetime_mmc[i]),
   draft = TRUE,
   root = orderly_root
 )
})
if (inherits(names, "list")) {
 names <- unlist(names)
}

# only run reports that haven't already been run
# iso3 <- iso3[is.na(names)]
pars <- pars[is.na(names), ]

# split these tasks into 6 parts
pars_splits <- split(
  seq_len(nrow(pars)),
  sort(seq_len(nrow(pars))) %% 3
)
pars <- pars[pars_splits[[1]], ]


possibly_batch <- purrr::possibly(orderly::orderly_batch, otherwise = NA)

# run fixed variance & covariance tasks 
ids <- orderly::orderly_batch(
  name = task,
  parameters = pars,
  root = orderly_root,
  continue_on_error = TRUE
)
