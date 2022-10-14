chunks_dir <- function(rs, attr) {
  require_class('simulator', rs, 'rs')
  result <- sprintf('%s/chunks-%s-%d', rs$root_dir, attr, rs$c_total)
  return(result)
}
