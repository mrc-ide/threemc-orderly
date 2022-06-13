# rerun countries which aren't working

# commit tasks that have worked though!!
tasks <- t$tasks
# success_tasks <- tasks[t$done]
success_tasks <- tasks
archived_ids <- orderly::orderly_list_archive(root = orderly_root)$id
for (i in seq_along(success_tasks)) {
  print(i)
  if (is.null(success_tasks[[i]])) next
  if (!"end" %in% success_tasks[[i]]$log()[[2]]) next
  x <- success_tasks[[i]]$wait(1)
  if (length(x) != 3) next
  if (x$id %in% archived_ids) next
  output <- strsplit(x$path, "\\\\")[[1]]
  output_filename <- output[length(output)]
  
  orderly::orderly_bundle_import(file.path(root, output_path, output_filename),
                                 root = orderly_root)
}
cr <-  orderly::orderly_list_archive(root = orderly_root)
cr <- cr[cr$name == "02_aggregations", ]
# reports that will need to be added by hand:
cr <- cr[
  !cr$id %in% c(
    "20220530-222035-4f5b1737",
    "20220530-222434-4ec27490",
    "20220530-233042-49ca0857",
    "20220530-233111-c4126a3b",
    "20220525-163856-8488b817",
    "20220530-233234-7a308bad"
  ), ]

cr <- cr %>% 
  filter(as.numeric(substr(id, 0, 8)) > 20220600)

lapply(seq_along(cr$id), function(x) orderly::orderly_push_archive(
  name = cr$name[x], id = cr$id[x], root = orderly_root)
)

# find unsuccessful countries, rerun these
# countries not yet finished
unsuccessful <- c()# iso3[-which(t$done)]
for (i in seq_along(tasks)) {
  if (t$done[i] == FALSE) {
    unsuccessful = c(unsuccessful, iso3[i])
  } else if (length(tasks[[i]]$result()) != 3) {
    unsuccessful = c(unsuccessful, iso3[i])
  }
} 
# set iso3 to unsuccessful and run orderly sequence 
iso3 <- unsuccessful