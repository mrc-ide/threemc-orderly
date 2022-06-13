# modelled countries
cntries <- c(
    "BDI", "ETH", "KEN", "MWI", "MOZ", "RWA", "TZA", "UGA", "ZMB", "ZWE", "AGO",
    "CMR", "TCD", "COG", "GAB", "ZAF", "SWZ", "LSO", "NAM", "BEN", "BFA", "CIV",
    "GIN", "MLI", "NER", "SEN", "SLE", "TGO")

# aggregated countries
# cntries <- c(
#     "LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE", "CIV",
#     "TGO", "ZMB", "ZAF", "AGO", "KEN", "ETH", "BFA", "CMR", "BDI")

orderly_root <- "~/imperial_repos/threemc-orderly"

aggregation_tasks <- list.dirs(
    paste0(orderly_root, "/src/"),
    full.names = FALSE,
    recursive = FALSE
)
aggregation_tasks <- aggregation_tasks[grepl("02", aggregation_tasks)]

modelling_grid <- expand.grid("iso3" = cntries, "names" = "01_modelling")
aggregation_grid <- expand.grid("iso3" = cntries, "names" = aggregation_tasks)
# grid <- rbind(modelling_grid, aggregation_grid)
grid <- modelling_grid

# download any missing reports
lapply(seq_len(nrow(grid)), function(row) {
    x <- grid[row, ]
    orderly::orderly_pull_archive(name = as.character(x$names),
                                  id = "latest(parameter:cntry == cntry)",
                                  parameters = list(cntry = as.character(x$iso3)),
                                  root = orderly_root)
})

# find the names for the reports of each row in grid
names <- lapply(seq_len(nrow(grid)), function (row) {
    x <- grid[row, ]
    orderly::orderly_search(
        name = as.character(x$names),
        query = "latest(parameter:cntry == cntry)",
        parameters = list(cntry = as.character(x$iso3)),
        root = orderly_root
    )
})

# find old tasks
old_names <- vapply(names, function(x) {
  as.numeric(substr(x, 0, 8)) < 20220525
  }, FUN.VALUE = logical(1))
cntries_old_mdl <- cntries[old_names]

# find missing reports, run these again!
missing_reports <- grid[is.na(names), ]
write.csv(missing_reports, "missing_reports.csv")


