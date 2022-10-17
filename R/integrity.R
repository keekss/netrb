integrity <- function(g, vcount_orig) {

  return(vcount(largest_component(g)) / vcount_orig)

}
