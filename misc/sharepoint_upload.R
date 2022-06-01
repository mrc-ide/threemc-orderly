orderly_root <- here::here()

# all mainland SSA countries (?)
iso3 <- c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "ETH", 
  "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LSO", "MLI", "MOZ", 
  "MWI", "NAM", "NER", "NGA", "RWA", "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", 
  "UGA", "ZAF", "ZMB", "ZWE"
)

# tasks to upload
tasks <- c("01_modelling", "02_aggregations")
# make grid with tasks for each country to upload
grid <- expand.grid("iso3" = iso3, "names" = tasks)

# orderly pull and push archive functions which don't fail w/ error
possibly_pull <- purrr::possibly(orderly::orderly_pull_archive, otherwise = NA)
possibly_push <- purrr::possibly(orderly::orderly_push_archive, otherwise = NA)

# download any missing reports for these countries
lapply(seq_len(nrow(grid)), function(row) {
  x <- grid[row, ]
  possibly_pull(name = as.character(x$names),
                # id = "latest(parameter:cntry == cntry)",
                id = "latest(parameter:cntry == cntry)",
                parameters = list(cntry = as.character(x$iso3)),
                root = orderly_root)
})

# push any reports on the present system not on sharepoint
lapply(seq_len(nrow(grid)), function(row) {
  x <- grid[row, ]
  possibly_push(
    name = as.character(x$names), root = orderly_root
  )
})

#### Alternative Push ###

# names <- lapply(seq_len(nrow(grid)), function (row) {
#   x <- grid[row, ]
#   orderly::orderly_search(
#     name = as.character(x$names),
#     query = "latest(parameter:cntry == cntry)",
#     parameters = list(cntry = as.character(x$iso3)),
#     root = orderly_root
#   )
# })
# names <- unlist(names)
# 
# files <- list.files("archive/01_modelling", recursive = "FALSE")
# n <- length(files)
# files <- c(
#   files,
#   list.files("archive/02_aggregations", recursive = "FALSE")
# )
# 
# push_grid <- data.frame("id" = files)
# push_grid$name <- "01_modelling"
# push_grid$name[(n + 1):nrow(push_grid)] <- "02_aggregations" 
# 
# 
# lapply(seq_len(nrow(push_grid)), function(row) {
#   x <- push_grid[row, ]
#   possibly_push(name = as.character(x$name),
#                 # id = "latest(parameter:cntry == cntry)",
#                 id = as.character(x$id),
#                 root = orderly_root)
# })
 
#### Find Missing Tasks #### 

names <- lapply(seq_len(nrow(grid)), function(row) {
  x <- grid[row, ]
  orderly::orderly_search(
    name = as.character(x$names),
    query = "latest(parameter:cntry == cntry)",
    parameters = list(cntry = as.character(x$iso3)),
    root = orderly_root
  )
})

names <- unlist(names)

# Missing and/or fit before 25/05/22:
missing_names <- which(is.na(names))
old_names <- which(as.numeric(substr(names, 0, 8)) <= 20220525)

run_tasks <- grid[-c(missing_names, old_names), ]
missing_tasks <- grid[missing_names, ]
old_tasks <- grid[old_names, ]