vdos <- function(
  del_attrs,
  sim    = NULL,
  graph  = NULL,
  recalc = FALSE
) {

  g <- sim_or_graph_arg('graph', sim = sim, graph = graph)

  result <- t(sapply(
    del_attrs,
    vdo,
    sim    = sim,
    graph  = g,
    recalc = recalc
  ))
  colnames(result) <- NULL
  return(result)
}
