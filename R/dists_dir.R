dists_dir <- function(rs, attr) {
  require_class('simulator', rs, 'rs')
  return(sprintf('%s/dists-%s', rs$root_dir, attr))
}
