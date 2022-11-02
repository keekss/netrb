match_metric_fun <- function(metric_fun_name) {

  log_library_choice <- function(choice) {
    logf('Using `%s::%s()` for `metric_vs_del()`.',
         choice,
         metric_fun_name)
  }
  # Search `netrb` for the function, then `igraph` if not in `netrb`.
  tryCatch(
    {
      metric_fun <- getFromNamespace(metric_fun_name, 'netrb')
      log_library_choice('netrb')
      return(metric_fun)
    },
    error = function(condition) {
      tryCatch(
        {
          metric_fun <- getFromNamespace(metric_fun_name, 'igraph')
          log_library_choice('igraph')
          return(metric_fun)
        },
        error = function(condition_2) {
          logf('Function name `%s()` not found in `netrb` or `igraph`.  Using function passed into `metric_vs_del()`.',
               metric_fun_name)
          return(NULL)
        }
      )
    }
  )
}
