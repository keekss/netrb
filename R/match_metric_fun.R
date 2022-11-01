match_metric_fun <- function(fun_name) {

  log_library_choice <- function(choice) {
    logf('Using `%s::%s()` for `fun_vs_del()`.',
         choice,
         fun_name)
  }
  # Search `netrb` for the function, then `igraph` if not in `netrb`.
  tryCatch(
    {
      FUN <- getFromNamespace(fun_name, 'netrb')
      log_library_choice('netrb')
      return(FUN)
    },
    error = function(condition) {
      tryCatch(
        {
          FUN <- getFromNamespace(fun_name, 'igraph')
          log_library_choice('igraph')
          return(FUN)
        },
        error = function(condition_2) {
          logf('Function `%s()` not found in `netrb` or `igraph`.  Attempting to match...',
               fun_name)
          return(match.fun(fun_name, descend = FALSE))
        }
      )
    }
  )
}
