#### Check Status of Cluster Tasks ####

# tabulate status for each cluster run
ts <- ls()[grepl("^t_", ls())]
ts <- ts[!ts == "t_x"]
lapply(ts, function(x) get(x)$status())

# parse for error (& success) messages (can join these with corresponding parameters)

# recursively search through logs for last line, giving error/success message
recurs_message <- function(x) {
  if (is.list(x) == FALSE) {
    return(x[[length(x)]])
  } else recurs_message(x[[length(x)]])
}

# search through each task in tasks message, tabulate final message
tabulate_messages <- function(tasks, preparse = TRUE) {
  
  tasks_orig <- tasks
  # remove tasks which are still pending; these will not have logs
  task_status <- vapply(tasks, function(x) x$status(), character(1))
  if (any(task_status == "PENDING")) tasks <- tasks[task_status != "PENDING"]
  
  messages <- vapply(tasks, function(x) {
    recurs_message(x$log())
  }, character(1))
  
  # add "PENDING" message for pending tasks
  if (length(tasks_orig) != length(tasks)) {
    n <- length(tasks_orig) - length(tasks)
    messages <- c(messages, rep("PENDING", n))
  }

  # Parse messages for similar final lines
  messages_preparsed <- messages
  # also change any messages with iter to iter
  messages[grepl("iter", messages)] <- "iter"
  # and i or ℹ to success (NOTE: Could be dangerous!)
  running_ms <- paste(c(
    "iter", 
    "halted", 
    "debugger", 
    "PENDING", 
    "Optimising...",
    "MakeADFun",
    "rw_order", 
    "inc_time_tmc"
  ), collapse = "|")
  
  messages[!grepl(running_ms, messages)] <- "success"
  
  return_obj <- messages
  if (preparse == TRUE) {
    return_obj <- list(
      "messages"  = messages, 
      "preparsed" = messages_preparsed
    )
  }
  return(return_obj) 
}

tasks <- ls()[grepl("^tasks_", ls())]
tasks <- tasks[tasks != c("tasks_x")]
messages <- lapply(tasks, function(x) tabulate_messages(get(x)))

lapply(messages, function(x) table(x$messages))
