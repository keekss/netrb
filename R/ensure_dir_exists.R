ensure_dir_exists <- function(.dir, must_be_empty = FALSE) {
  if (dir.exists(.dir)) {
    if (all(must_be_empty,
            length(list.files(.dir)) > 0)) stop(sprintf(
        '`%s` exists and is not empty.  If continuing simulations for same `simulator` name and same original graph, pass `from_scratch == FALSE` to `simulator()`.',
        .dir))
  } else {
    logf('Creating directory `%s`.', .dir)
    dir.create(.dir, recursive = TRUE)
  }
}
