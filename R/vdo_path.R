vdo_path <- function(sim, .attr) {

  return(sprintf('%s/vdos/%s.csv',
                 sim$root_dir,
                 .attr))
}
