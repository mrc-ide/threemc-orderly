
# directory to save contexts
root <- "~/net/home/circumcision-coverage-orderly/contexts"
threemc::create_dirs_r(root)
config <- didehpc::didehpc_config(workdir = root)

# path to save bundles
path_bundles <- file.path(root, "bundles")

# directory to save bundled/zipped orderly tasks
save_loc <- paste0(root, "/bundles")
# check if dir (and parent dirs) for "save_loc" exists, create if not
threemc::create_dirs_r(save_loc)

#### Contexts 

# change directory to cluster
setwd(dirname(root))

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
    # paths,
    "github::paddy7wb/anclik",
    # "github::jeffeaton/anclik/anclik",
    "github::paddy7wb/epp@local_anclik",
    "github::paddy7wb/eppasm@local_epp",
    "github::paddy7wb/naomi@local_eppasm",
    "github::mrc-ide/threemc@local_naomi"
  ))
)
obj <- didehpc::queue_didehpc(context = ctx, config = config)

#### Bundle orderly tasks

# reset directory to circumcision-coverage-orderly (bit hackish as well!)
setwd(here::here())

# countries  
# iso3 <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", 
#           "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "HTI", 
#           "KEN", "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", 
#           "RWA", "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZMB", 
#           "ZWE", "ZAF")
# iso3 <- c("LSO", "SWZ", "NAM")

# pack up 01_modelling for each country
bundles <- orderly::orderly_bundle_pack(path_bundles, "01_modelling",
                            parameters = list(cntry = "LSO"))
# bundles <- lapply(iso3, function(x) orderly::orderly_bundle_pack(path_bundles,
#                                                       "01_modelling",
#                                                       parameters = list(
#                                                         cntry = x
#                                                       )))

#### Run Bundles ####
path <- paste(last(strsplit(dirname(bundles$path), "/")[[1]]), 
      basename(bundles$path), sep = "/")
# paths <- lapply(bundles, function(x) {
#   file.path(path_bundles, basename(x$path))
# })
# output_path <- "output"
t <- obj$enqueue(orderly::orderly_bundle_run(
 path 
))
# t <- obj$lapply(paths, orderly::orderly_bundle_run)

