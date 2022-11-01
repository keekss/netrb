integrity <- function(
  sim   = NULL,
  graph = NULL,
  vcount_orig = NULL
) {

  g <- sim_or_graph_arg('graph', sim = sim, graph = graph)

  if (is.null(vcount_orig)) {
    if (!is.null(sim)) vcount_orig <- vcount(sim$g_orig)
    else stop(
      'Must specify simulator or original vertex count.'
    )
  }

  # Return the ratio of largest component size to original graph size.
  return(vcount(largest_component(g)) / vcount_orig)

}
