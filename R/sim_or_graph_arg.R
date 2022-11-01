sim_or_graph_arg <- function(
    param,
    sim   = NULL,
    graph = NULL,
    diam  = NULL,
    unconn_dist = NULL) {

  if (is.null(sim) && is.null(g)) stop('Must specify simulator or graph.')
  else if (param == 'graph') return(
    if (!is.null(graph)) graph else sim$g_orig
  )
  else if (param == 'g_orig') return(
    if (!is.null(sim)) sim$g_orig else duplicate(g)
  )
  else if (param == 'diam') {
    if (!is.null(diam)) return(diam)
    # `sim` should always have a diameter.
    else if (!is.null(sim)) return(sim$diam)
    else {
      # print('Manually calculating diameter.  It is recommended to pre-calculate this for large graphs.')
      g <- sim_or_graph_arg('graph', sim = sim, graph = graph)
      print(class(g))
      return(diameter(g))
    }
  } else if (param == 'unconn_dist') {
    # print('Using default unconnected distance of `diameter(g) + 1`.')
    if (!is.null(diam)) return(diam + 1)
    else {
      diam <- sim_or_graph_arg('diam', sim = sim, graph = graph)
      return(diam + 1)
    }
  } else stop('Invalid parameter: ', param)
}
