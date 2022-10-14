ensure_dir_exists <- function(.dir, must_be_empty = FALSE) {
  if (dir.exists(.dir)) {
    if (must_be_empty) {
      if (length(list.files(.dir)) > 0) stop(sprintf(
        '`%s` exists and is not empty.  If continuing simulations for same `rb_simulator` name and same original graph, pass `name_is_new == TRUE` to `rb_simulator()`.', .dir
      ))
    }
  }
  else {
    logf('Creating directory `%s`.', .dir)
    dir.create(.dir, recursive = TRUE)
  }
}
