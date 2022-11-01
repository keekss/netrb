sim_or_graph_arg <- function(
    param,
    sim   = NULL,
    graph = NULL,
    diam  = NULL,
    unconn_dist = NULL) {

  if (is.null(sim) && is.null(g)) stop('Must specify simulator or graph.')
  else if (param == 'graph') return(
    if (!is.null(graph)) graph else sim$g
  )
  else if (param == 'diam') {
    if (!is.null(diam)) return(diam)
    # `sim` should always have a diameter.
    else if (!is.null(sim$diam)) return(sim$diam)
    else {
      print('Manually calculating diameter.  It is recommended to pre-calculate this for large graphs.')
      g <- sim_or_graph_arg('graph', sim = sim, graph = graph)
      return(diameter(g))
    }
  } else if (param == 'unconn_dist') {
    print('Using default unconnected distance of `diameter(g) + 1`.')
    diam <- sim_or_graph_arg('diam', sim = sim, graph = graph)
    return(diam + 1)
  } else stop('Invalid parameter: ', param)
}
