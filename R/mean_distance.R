mean_distance <- function(
    sim   = NULL,
    graph = NULL,
    diam  = NULL,
    unconn_dist = NULL) {

  g <- sim_or_graph_arg('graph', sim = sim, graph = graph)
  if (is.null(unconn_dist)) {
    unconn_dist <- sim_or_graph_arg(
      'unconn_dist',
      sim  = sim,
      graph = graph,
      diam  = diam
    )
  }

  dists <- netrb::distances(
    sim   = sim,
    graph = g,
    unconn_dist = unconn_dist
  )

  return(mean(dists, na.rm = TRUE))

}
