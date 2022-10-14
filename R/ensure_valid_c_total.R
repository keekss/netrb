ensure_valid_c_total <- function(c_total) {
  if (1/c_total != round(1/c_total, 4)) stop(
    '\n  Invalid number of chunks: ', c_total,
    '\n  Number of chunks must divide 1000.'
  )
}
