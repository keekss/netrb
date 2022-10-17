delete_simulator <- function(sim) {
  logf('Deleting simulator `%s`', sim$.name)
  unlink(sim$root_dir, recursive = TRUE)
  rm(sim, envir = globalenv())
}
