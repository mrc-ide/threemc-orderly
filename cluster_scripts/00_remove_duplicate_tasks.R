# check for duplicate tasks

#### initial ####

library(orderly)
library(dplyr)
library(tidyr)

# task <- "01final_modelling"
task <- "01efinal_ppc_models"
# task <- "01e_new_final_ppc_models"

# VMMC countries  
vmmc_iso3 <- c(
  "LSO", "MOZ", "NAM", "RWA", "TZA", "UGA", "MWI",
  "SWZ", "ZWE", "ZMB", "ETH", "KEN", "ZAF" # LSO, SWZ ran already
)
no_type_iso3 <- c("LBR", "SEN", "NER", "GIN", "COD")

iso3 <- c("LSO", "MWI", "MOZ", "NAM", "RWA", "SWZ", "TZA", "UGA", "ZWE",
          "ZMB", "COG", "AGO", "BEN", "BFA", "BDI", "CMR", "TCD", "CIV",
          "GAB", "GIN", "MLI", "NER", "TGO", "SEN", "SLE", "KEN", "ETH",
          "ZAF", "LBR", "GHA", "GMB", "NGA", "COD")
# iso3 <- iso3[!iso3 %in% c(vmmc_iso3, no_type_iso3)]
iso3 <- iso3[!iso3 %in% c(no_type_iso3)]

rw_order <- c(0, 1, 2)
paed_age_cutoff <- c(Inf, 10)
inc_time_tmc <- c(FALSE, TRUE)

# parameters for vmmc countries 
# vmmc_par_df <- crossing(
#   cntry = vmmc_iso3, 
#   rw_order, 
#   paed_age_cutoff, 
#   inc_time_tmc
# )
# vmmc_par_df <- data.frame(
#   cntry           = vmmc_iso3, 
#   rw_order        = 0, 
#   paed_age_cutoff = 10, 
#   inc_time_tmc    = TRUE
# )

pars_df <- bind_rows(
  # vmmc_par_df,
  crossing(cntry = iso3, rw_order, paed_age_cutoff, inc_time_tmc)
  # data.frame(cntry           = iso3, 
  #            rw_order        = 0, 
  #            paed_age_cutoff = Inf, 
  #            inc_time_tmc    = TRUE)
)

# add countries with no type
pars_df <- bind_rows(
  pars_df, 
  tidyr::crossing(
    cntry = no_type_iso3, 
    rw_order = c(0, 1, 2), 
    paed_age_cutoff = Inf, 
    inc_time_tmc = FALSE
  )
)

# pars_df_1 <- readr::read_csv("~/OneDrive/vmmc_remaining_01e.csv", col_select = -1)
# pars_df_2 <- readr::read_csv("~/OneDrive/non_vmmc_remaining_01e.csv")
# pars_df <- bind_rows(pars_df_1, pars_df_2)

# only run for parameters with successful models
(names <- lapply(seq_len(nrow(pars_df)), function(i) {
  message(100 * (i / nrow(pars_df)), "% completed")
  is_paper <- TRUE
  if (pars_df$cntry[i] %in% c("UGA", "MWI")) {
    is_paper <- FALSE
  }
  orderly::orderly_search(
    # name = "01final_modelling",
    name = task,
    # query = "latest(
    query = "(
      parameter:cntry == cntry && parameter:is_paper == is_paper &&
      parameter:rw_order == rw_order && parameter:paed_age_cutoff == paed_age_cutoff
      && parameter:inc_time_tmc == inc_time_tmc
    )",
    parameters = list(
      cntry                  = pars_df$cntry[i],
      is_paper               = is_paper,
      rw_order               = pars_df$rw_order[i],
      paed_age_cutoff        = pars_df$paed_age_cutoff[i],
      inc_time_tmc           = pars_df$inc_time_tmc[i]
    ),
    root = orderly_root
  )
}))

# pars_df <- pars_df[!is.na(names), ]
# Take lengths of names (i.e. results of orderly_search for each task)
n_ran <- unlist(lapply(names, length))
# pull pars for tasks yet to be run
unran_tasks <- pars_df[n_ran == 0, ] 

# For duplicated tasks, pull older versions; want to delete these
duplicate_tasks <- names[n_ran > 1]
rm_tasks <- unlist(lapply(duplicate_tasks, function(x) {
  n <- length(x)
  return(x[seq_len(n - 1)])
}))

# remove these tasks; they are superfluous to requirements
lapply(rm_tasks, function(x) {
  # print(
  system(
    # paste0("mv ~/01final_modelling/", x, " ~/01final_modelling/dup_tasks/.")
    # paste0("mv ~/01efinal_ppc_models/", x, " ~/01efinal_ppc_models/dup_tasks/.")
    # paste0("mv ~/01e_new_final_ppc_models/", x, " ~/01e_new_final_ppc_models/dup_tasks/.")
    paste0("mv ~/", task, "/", x, " ~/", task, "/dup_tasks/.")
  )
})

# do the same for zip files
zip_rm_tasks <- paste0(rm_tasks, ".zip")
lapply(zip_rm_tasks, function(x) {
  # print(
  system(
    # paste0("mv ~/01final_modelling/", x, " ~/01final_modelling/dup_tasks/.")
    # paste0("mv ~/01efinal_ppc_models/", x, " ~/01efinal_ppc_models/dup_tasks/.")
    # paste0("mv ~/01e_new_final_ppc_models/", x, " ~/01e_new_final_ppc_models/dup_tasks/.")
    paste0("mv ~/", task, "/zips/", x, " ~/", task, "/dup_zips/.")
  )
})


# repeat the above to ensure you've correctly removed duplicates!
