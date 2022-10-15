attr_order_path <- function(rs, .attr) {
  # TODO add to docu how this is to standardize file paths
  require_class('simulator', rs, 'rs')

  return(sprintf('%s/%s.csv',
                 attr_orders_dir(rs),
                 .attr))
}
