dists_dir <- function(rs, attr) {
  require_class('simulator', rs, 'sim')
  return(sprintf('%s/dists-%s', rs$root_dir, attr))
}
