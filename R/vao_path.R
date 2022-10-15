vao_path <- function(sim, .attr) {
  # TODO add to docu how this is to standardize file paths
  require_class('simulator', sim, 'sim')

  return(sprintf('%s/vaos/%s.csv',
                 sim$root_dir,
                 .attr))
}
