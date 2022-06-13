#### Initial Orderly Repo Setup ####

# initialise orderly repo
# orderly::orderly_init(root = "~/imperial_repos/threemc-orderly/")

# Change directory to this directory
# setwd("~/imperial_repos/threemc-orderly/")

# generate orderly-specific gitignore 
# orderly::orderly_use_gitignore(prompt = FALSE)

# create a directory in src from which to run modelling for LSO
# orderly::orderly_new("01_modelling")

# fill in ordelry.yml with artefacts, parameters, global resources, etc
# write out script to be run

library(dplyr)
library(data.table)

#### Committing Reports: Function #### 
orderly_commits <- function(type = "draft") {
  
  # list draft reports
  if (type == "draft") {
    (dr <- orderly::orderly_list_drafts())
    
    # commit the latest reports
    dr <- dr %>% 
      group_by(name) %>% 
      summarise(id = last(id), .groups = 'drop')
    print(dr)
    lapply(dr$id, orderly::orderly_commit)
      
    # push to sharepoint
    lapply(dr$name, orderly::orderly_push_archive)
    
  } else if (type == "archive") {
    (cr <- orderly::orderly_list_archive())
    # cr <- cr %>% 
    #   filter(as.numeric(substr(cr$id, 0, 8)) > 20220600)
    lapply(seq_len(nrow(cr)), function(x) orderly::orderly_push_archive(
      name = cr$name[x], id = cr$id[x])
    )
  }
}

#### Run src Scripts ####

# set country parameter
# iso3 <- "LSO"

# take directories/orderly tasks in src
dirs <- list.dirs(path = "src", full.names = FALSE, recursive = FALSE)

## modelling
orderly::orderly_run("01_modelling", list(cntry = iso3))

# commit 
orderly_commits()

## aggregations
# aggregation orderly tasks
aggregations <- dirs[dirs %like% "02"]
lapply(aggregations, orderly::orderly_run, list(cntry = iso3))

# commit 
orderly_commits()

## plots
# plotting orderly tasks
# plots <- dirs[dirs %like% "03"]
# lapply(plots, orderly::orderly_run, list(cntry = iso3))

# commit 
# orderly_commits()