set_c_total <- function(rs, c_total) {
  ensure_valid_c_total(c_total = c_total)

  logf('Setting total chunks: %d.  Previous: %d.',
       c_total,
       rs$c_total)

  rs$c_total <- c_total

  return(rs)
}
