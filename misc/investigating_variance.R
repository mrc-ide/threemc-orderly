orderly_root <- here::here()
# only missing for TZA
iso3 <- c("COG", "SLE", "BEN", "LSO", "MWI", "MOZ", "NAM", "RWA", "TZA", "UGA", "ZWE", "ZMB", "ZAF")

tasks <- "01a_modelling_fixed_pars"

grid <- expand.grid("iso3" = iso3, "names" = tasks)

possibly_pull <- purrr::possibly(orderly::orderly_pull_archive, otherwise = NA)

# download any missing reports
lapply(seq_len(nrow(grid)), function(row) {
  x <- grid[row, ]
  possibly_pull(name = as.character(x$names),
                                # id = "latest(parameter:cntry == cntry)",
                                id = "latest(parameter:cntry == cntry &&
                                parameter:logsigma_time_mmc_val == logsigma_time_mmc_val &&
                                parameter:logsigma_spacetime_mmc_val == logsigma_spacetime_mmc_val &&
                                parameter:logsigma_agetime_mmc_val == logsigma_time_mmc_val)",
                                parameters = list(cntry = as.character(x$iso3),
                                                  logsigma_time_mmc_val = 0.32550794,
                                                  logsigma_spacetime_mmc_val = -2.30655943,
                                                  logsigma_agetime_mmc_val = 1.82913044),
                                root = orderly_root)
})

lapply(seq_len(nrow(grid)), function(row) {
  x <- grid[row, ]
  possibly_pull(name = as.character(x$names),
                # id = "latest(parameter:cntry == cntry)",
                id = "latest(parameter:cntry == cntry)",
                parameters = list(cntry = as.character(x$iso3),
                                  logsigma_time_mmc_val = 0.32550794,
                                  logsigma_spacetime_mmc_val = -2.30655943,
                                  logsigma_agetime_mmc_val = 1.82913044),
                root = orderly_root)
})



# find the names for the reports of each row in grid
names <- lapply(seq_len(nrow(grid)), function (row) {
  x <- grid[row, ]
  orderly::orderly_search(
    name = as.character(x$names),
    query = "latest(parameter:cntry == cntry &&
    parameter:logsigma_time_mmc_val == logsigma_time_mmc_val &&
    parameter:logsigma_spacetime_mmc_val == logsigma_spacetime_mmc_val &&
    parameter:logsigma_agetime_mmc_val == logsigma_time_mmc_val)",
    parameters = list(cntry = as.character(x$iso3),
                      logsigma_time_mmc_val = 0.32550794,
                      logsigma_spacetime_mmc_val = -2.30655943,
                      logsigma_agetime_mmc_val = 1.82913044),
    root = orderly_root
  )
})




orderly::orderly_search("01a_modelling_fixed_pars", 
                        id = "latest(parameter:cntry == cntry)", 
                        parameters = list(cntry = iso3))

lapply(iso3, function(x) {
  orderly::orderly_pull_archive(name = "01_modelling", 
                                id = 
                                  "latest(parameter:cntry == cntry)", 
                                parameters = list(cntry = x))
})
