to_idx <- function(size, arr_len) {

  # TODO rename this?
  # g <- if (is.na(graph)) sim$g_orig else graph

  if (is.na(size)) return(arr_len)
  else {
    result <- round(if (size < 1) size*arr_len else size)
    if (result < 1 || result > arr_len) stop(sprintf(
      'Invalid index `%d` for sample size `%d`. Index must be between 1 and the length of the array, inclusive.',
      result,
      size
    ))
    return(result)
  }
}
