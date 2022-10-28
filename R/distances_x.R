distances_x <- function(sim,
                      v,
                      to) {
  # TODO for other del_states
  result <- igraph::distances(graph = sim$g_orig,
                              v = v,
                              to = to)

  result[result == Inf] <- sim$unconn_dist

  return(result)
}
