verify_function <- function(func_name) {

  log_library_choice <- function(.library) {
    logf('Using `%s::%s()` for `fun_vs_del()`.',
         .library,
         func_name)
  }
  # Search `netrb` for the function, then `igraph` if not in `netrb`.
  tryCatch(
    {
      func <- getFromNamespace(func_name, 'netrb')
      log_library_choice('netrb')
      return(func)
    },
    error = function(condition) {
      tryCatch(
        {
          func <- getFromNamespace(func_name, 'igraph')
          log_library_choice('igraph')
          return(func)
        },
        error = function(condition_2) {
          logf('Function `%s()` not found in `netrb` or `igraph`.  Proceeding...',
               func_name)
        }
      )
    }
  )
}
