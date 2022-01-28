#### Initial Orderly Repo Setup ####

# initialise orderly repo
# orderly::orderly_init(root = "~/imperial_repos/circumcision-coverage-orderly/")

# Change directory to this directory
# setwd("~/imperial_repos/circumcision-coverage-orderly/")

# generate orderly-specific gitignore 
# orderly::orderly_use_gitignore(prompt = FALSE)

# create a directory in src from which to run modelling for LSO
# orderly::orderly_new("01_modelling")

# fill in ordelry.yml with artefacts, parameters, global resources, etc
# write out script to be run

#### Run src Scripts ####

# set country parameter
iso3 <- "LSO"

# take directories/orderly tasks in src
dirs <- list.dirs(path = "src", full.names = FALSE)

## modelling
orderly::orderly_run("01_modelling", list(cntry = iso3))

## aggregations
# aggregation orderly tasks
aggregations <- dirs[dirs %like% "02"]
lapply(aggregations, orderly::orderly_run, list(cntry = iso3))

## plots
# plotting orderly tasks
# plots <- dirs[dirs %like% "03"]
# lapply(plots, orderly::orderly_run, list(cntry = iso3))

#### Committing Reports #### 

# list draft reports
(dr <- orderly::orderly_list_drafts())

# commit the latest reports
dr <- dr %>% 
  group_by(name) %>% 
  summarise(id = last(id), .groups = 'drop')
print(dr)
lapply(dr$id, orderly::orderly_commit)

# push to sharepoint
lapply(dr$name, orderly::orderly_push_archive)
