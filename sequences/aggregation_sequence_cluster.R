#### Initial ####

# directory to save contexts (change as appropriate)
root <- "~/net/home/threemc-orderly/contexts"
# ensure it exists, and if not, create it
threemc::create_dirs_r(root)

# orderly repos with tasks to be bundled
orderly_root <- here::here()

# path to save bundles
path_bundles <- file.path(root, "bundles")
threemc::create_dirs_r(path_bundles)

# path to output bundles
output_path <- "output"
threemc::create_dirs_r(output_path)

# working countries (run in batches due to space constraints)
iso3 <- # c("LSO", "MWI")
        # c("MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE")
        # c("ZMB", "COG", "AGO", "BEN") 
        # c("BFA", "BDI", "CMR", "TCD")
        # c("CIV", "GAB", "GIN", "MLI")
        # c("NER", "TGO", "SEN", "SLE")

# iso3 <- # c("LSO", "MWI", "NAM", "RWA")
  # c("MOZ", "SWZ", "TZA", "UGA")
  # c("ZMB", "ZWE", "COG", "AGO")
  # c("BEN", "BFA", "BDI", "CMR") 
  # c("TCD", "CIV", "BFA", "BDI") 
  # c("CMR", "TCD", "CIV", "GAB")
  # c("BFA", "BDI", "CMR", "CIV")
  # c("GIN", "MLI", "NER")
  # c("TGO", "SEN", "SLE")

iso3 <- c("LSO", "MWI", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE", "ZMB", "COG", 
          "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV", "GAB", "GIN", "MLI",
          "SLE", "ZAF", "SEN", "TGO", "NER", "ETH", "MOZ", "KEN")


# pull through orderly remotes for each country
lapply(iso3, function(x) {
  orderly::orderly_pull_archive(name = "01_modelling", 
                                id = "latest(parameter:cntry == cntry)", 
                                parameters = list(cntry = x))
})


#### Bundle orderly tasks #### 

# pack up aggregations for each country
dirs <- list.dirs(path = "src", full.names = FALSE, recursive = FALSE)
aggregation_bundles <- dirs[grepl("02", dirs)]
if (length(iso3) == 1) {
#   bundles <- orderly::orderly_bundle_pack(path_bundles,
#                                           "01_modelling",
#                                           parameters = list(cntry = iso3),
#                                           root = orderly_root)
  bundles <- lapply(aggregation_bundles, function(x) {
    orderly::orderly_bundle_pack(path_bundles,
                                 x, 
                                 parameters = list(
                                   cntry = iso3
                                 ),
                                 root = orderly_root)
  })
} else {
  
  # parameters to run for
  grid <- expand.grid("iso3" = iso3, "name" = aggregation_bundles)
  bundles <- lapply(seq_len(nrow(grid)), function(row) {
    x <- grid[row, ]
    orderly::orderly_bundle_pack(
      path_bundles, 
      as.character(x[["name"]]), 
      parameters = list(cntry = as.character(x[["iso3"]])),
      root = orderly_root
    )
  })
}


#### Contexts ####

# change directory to cluster
# setwd(dirname(root))

# cluster config
config <- didehpc::didehpc_config(workdir = root)

# setup context for orderly task (packages required, etc)
ctx <- context::context_save(
  root,
  packages = c(
    "dplyr", "sf", "data.table", "Matrix", "TMB", "rlang", "ggplot2", "orderly",
    "R.utils", "RcppEigen", "stringr", "naomi"
  ),
  package_sources = conan::conan_sources(c(
    "github::mrc-ide/first90release",
    "github::mrc-ide/threemc@feature/add_tmb"
  ))
)
# queue above context on cluster
obj <- didehpc::queue_didehpc(context = ctx, config = config)


#### Run Bundles ####

if (length(bundles) == 1) {
  if (is.list(bundles[[1]])) bundles <- rlang::flatten(bundles)
  # (relative) paths to bundles
  paths <- paste(last(strsplit(dirname(bundles$path), "/")[[1]]),
                 basename(bundles$path), sep = "/")
  
  # send orderly task to cluster!
  t <- obj$enqueue(orderly::orderly_bundle_run(
    path = paths, workdir = output_path
  ))
} else {
  # (relative) paths to bundles
  paths <- lapply(bundles, function(x) {
    paste(last(strsplit(dirname(x$path), "/")[[1]]), 
          basename(x$path), sep = "/")
  })
  
  # send orderly tasks to cluster! Wait till they have all run before proceeding
  t <- obj$lapply(paths, orderly::orderly_bundle_run, workdir = output_path)
  
  # look at individual tasks for logs
  tasks <- t$tasks
}


#### Import Completed Tasks ####

if (length(iso3) == 1 | length(bundles) == 1) {
  output <- strsplit(t$wait(100)$path, "\\\\")[[1]]
  output_filename <- output[length(output)]
  orderly::orderly_bundle_import(file.path(root, output_path, output_filename),
                                 root = orderly_root)
} else {
  for (output in t$wait(100)) {
    out <- strsplit(output$path, "\\\\")[[1]]
    output_filename <- out[length(out)]
    orderly::orderly_bundle_import(file.path(root, output_path, output_filename),
                                   root = orderly_root)
  }
}
  
# push committed reports to sharepoint folder
cr <-  orderly::orderly_list_archive(root = orderly_root)
lapply(seq_along(cr$id), function(x) orderly::orderly_push_archive(
  name = cr$name[x], id = cr$id[x], root = orderly_root)
)
  
