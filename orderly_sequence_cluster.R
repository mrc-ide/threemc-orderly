#### Initial ####

# directory to save contexts
root <- "~/net/home/circumcision-coverage-orderly/contexts"
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

#### Bundle orderly tasks #### 

# countries  
# iso3 <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", 
#           "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "HTI", 
#           "KEN", "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", 
#           "RWA", "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZMB", 
#           "ZWE", "ZAF")
# iso3 <- c("LSO", "SWZ", "NAM")

# pack up 01_modelling for each country
bundles <- orderly::orderly_bundle_pack(path_bundles, 
                                        "01_modelling",
                                        parameters = list(cntry = "LSO"), 
                                        root = orderly_root)
# bundles <- lapply(iso3, function(x) orderly::orderly_bundle_pack(path_bundles,
#                                                       "01_modelling",
#                                                       parameters = list(
#                                                         cntry = x
#                                                       )))

#### Contexts ####

# change directory to cluster
# setwd(dirname(root))

# cluster config
config <- didehpc::didehpc_config(workdir = root)

# build remotes locally (change this code to an lapply!)
# build_path <- dirname(here::here())
# paths <- paste0(build_path, c("anclik/", "epp/", "eppasm/", "naomi/"))
# lapply(paths, pkgbuild::build, dest_path = dirname("root"), vignettes = FALSE)

ctx <- context::context_save(
  root,
  packages = c(
    "dplyr", "sf", "data.table", "Matrix", "TMB", "rlang", "ggplot2", "orderly",
    "R.utils", "RcppEigen"
  ),
  package_sources = conan::conan_sources(c(
    "github::mrc-ide/first90release",
    "github::mrc-ide/threemc@feature/add_tmb"
  ))
)
obj <- didehpc::queue_didehpc(context = ctx, config = config)


#### Run Bundles ####

path <- paste(last(strsplit(dirname(bundles$path), "/")[[1]]), 
              basename(bundles$path), sep = "/")
# paths <- lapply(bundles, function(x) {
#   file.path(path_bundles, basename(x$path))
# })
t <- obj$enqueue(orderly::orderly_bundle_run(
  path = path, workdir = output_path
))
# t <- obj$lapply(paths, orderly::orderly_bundle_run)

#### Import Completed Tasks ####

output <- strsplit(t$wait(100)$path, "\\\\")[[1]]
output_filename <- output[length(output)]
orderly::orderly_bundle_import(file.path(root, output_path, output_filename),
                               root = orderly_root)

