to_idx <- function(size, vec_len) {

  # If size is a fraction (between 0 and 1),
  # normalize it to the range [1, vec_len].
  if (size < 1) {
    size <- normalize_vec(
      vec = c(0, size, 1),
      min_final = 1,
      max_final = vec_len
    )[2]
  }

  result <- round(size)

  if (result < 1 || result > vec_len) stop(sprintf(
    'Invalid index `%d` for sample size `%d`. Index must be between 1 and the length of the array, inclusive.',
    result,
    size
  ))
  return(result)
}
