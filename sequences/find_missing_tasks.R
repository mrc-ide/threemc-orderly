# modelled countries
cntries <- c(
    "BDI", "ETH", "KEN", "MWI", "MOZ", "RWA", "TZA", "UGA", "ZMB", "ZWE", "AGO",
    "CMR", "TCD", "COG", "GAB", "ZAF", "SWZ", "LSO", "NAM", "BEN", "BFA", "CIV",
    "GIN", "MLI", "NER", "SEN", "SLE", "TGO")

# aggregated countries
cntries <- c(
    "LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE", "CIV",
    "TGO", "ZMB", "ZAF", "AGO", "KEN", "ETH", "BFA", "CMR", "BDI")

orderly_root <- "~/imperial_repos/circumcision-coverage-orderly"

aggregation_tasks <- list.dirs(
    paste0(orderly_root, "/src/"),
    full.names = FALSE,
    recursive = FALSE
)
aggregation_tasks <- aggregation_tasks[grepl("02", aggregation_tasks)]

grid <- expand.grid("iso3" = cntries, "names" = aggregation_tasks)

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

# find missing reports, run these again!
missing_reports <- grid[is.na(names), ]
write.csv(missing_reports, "missing_reports.csv")

