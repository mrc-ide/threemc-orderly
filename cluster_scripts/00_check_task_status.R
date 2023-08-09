#### Check Status of Cluster Tasks ####

# tabulate status for each cluster run
table(t_32_tmbad$status())
table(t_32_cppad$status())
table(t_mrc_tmbad$status())
table(t_mrc_cppad$status())


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

m_32_tmbad <- tabulate_messages(tasks_32_tmbad)
m_32_cppad <- tabulate_messages(tasks_32_cppad)
m_mrc_tmbad <- tabulate_messages(tasks_mrc_tmbad)
m_mrc_cppad <- tabulate_messages(tasks_mrc_cppad)

table(m_32_tmbad$messages)
table(m_32_cppad$messages)
table(m_mrc_tmbad$messages)
table(m_mrc_cppad$messages)

