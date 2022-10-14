chunk_path <- function(rs, attr, c_name) {
  return(sprintf(
    '%s/%s.csv',
    chunks_dir(rs, attr),
    c_name
  ))
}
