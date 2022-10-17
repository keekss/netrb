verify_function <- function(func_name) {

  log_library_choice <- function(.library) {
    logf('Using `%s::%s()` for `func_vs_deletions()`.',
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
          stop(sprintf('Function `%s()` not found in `netrb` or `igraph`.',
                       func_name))
        }
      )
    }
  )
}
