write_chunks <- function(rs, attr = 'rand') {
  # Get vertex ids ordered by attribute.
  .attr_ranks <- attr_ranks(rs, attr = attr)
  # Split into partitions.
  # Referenced https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks.
  # Only works on numeric vectors.
  c_total <- rs$c_total
  chunks <- split(.attr_ranks, sort(.attr_ranks %% c_total))
  .chunks_dir <- chunks_dir(rs, attr = attr, c_total = c_total)
  ensure_dir_exists(.chunks_dir)

  # Write chunks to file.
  for (i in 1:c_total) {
    .path <- sprintf('%s/%s.csv', .chunks_dir, i)
    fwrite(as.list(chunks[[i]]), file = .path)
  }
  return(chunks)
}
