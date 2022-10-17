chunk_end_indices <- function(arr_len,
                              nchunks,
                              start = 1,
                              end = NA) {
  if (is.na(end)) end <- arr_len

  start <- to_idx(size = start, arr_len = arr_len)
  end   <- to_idx(size = end,   arr_len = arr_len)

  result <- (start - 1) + 1:nchunks * (end - start + 1) / nchunks

  return(sapply(result, round))
}
