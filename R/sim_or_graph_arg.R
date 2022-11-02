sim_or_graph_arg <- function(
  param,
  sim   = NULL,
  graph = NULL,
  diam  = NULL,
  unconn_dist = NULL
) {

  if (is.null(sim) && is.null(graph)) stop('Must specify simulator or graph.')

  # If a simulator is passed, all relevant parameters
  # have been initialized.
  if (!is.null(sim)) return(switch(
    param,
    'graph' = sim$g_orig,
    'diam'  = sim$diam_orig,
    'unconn_dist' = sim$unconn_dist,
  ))
  else {
    # Otherwise, a graph must have been passed.
    if (param == 'graph') return(graph)
    else if (param == 'diam') {
      # print('Manually calculating diameter.  It is recommended to pre-calculate this for large graphs.')
      return(diameter(g))
    } else if (param == 'unconn_dist') {
      # print('Using default unconnected distance of `diameter(g) + 1`.')
      diam <- sim_or_graph_arg('diam', graph = graph)
      return(diam + 1)
    }
    else stop('Invalid parameter: ', param)
  }
}
