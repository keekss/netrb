chunk_end_indices <- function(
  vec_len,
  nchunks,
  start = 1,
  end   = vec_len
) {

  start <- to_idx(size = start, vec_len = vec_len)
  end   <- to_idx(size = end,   vec_len = vec_len)

  result <- seq(from = start,
                to   = end,
                by   = (end - start) / nchunks)

  return(round(result)[-1])
}
