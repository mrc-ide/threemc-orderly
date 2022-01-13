# initialise orderly repo
# orderly::orderly_init(root = "~/imperial_repos/circumcision-coverage-orderly/")

# Change directory to this directory
# setwd("~/imperial_repos/circumcision-coverage-orderly/")

# generate orderly-specific gitignore 
# orderly::orderly_use_gitignore(prompt = FALSE)

# create a directory in src from which to run modelling for LSO
# orderly::orderly_new("01_LSO_model")

# fill in ordelry.yml with artefacts, parameters, global resources, etc
# write out script to be run
# run script!
orderly::orderly_run("01_LSO_model", list(cntry = "LSO"))

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
