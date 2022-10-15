attr_orders_dir <- function(rs) {
  # TODO add to docu how this is to standardize file paths
  require_class('simulator', rs, 'rs')

  return(sprintf('%s/attr_orders', rs$root_dir))
}
