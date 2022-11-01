distances <- function(
    sim   = NULL,
    graph = NULL,
    v     = NULL,
    to    = NULL,
    diam  = NULL,
    unconn_dist = NULL,
    zero_as_NA  = TRUE) {

  g <- sim_or_graph_arg('graph', sim = sim, graph = graph)
  if (is.null(unconn_dist)) {
    unconn_dist <- sim_or_graph_arg('unconn_dist', diam = diam)
  }

  if (is.null(v))  v  <- V(g)
  if (is.null(to)) to <- V(g)

  result <- igraph::distances(
    graph = g,
    v     = v,
    to    = to
  )

  result[result == Inf] <- unconn_dist

  if (zero_as_NA) result[result == 0] <- NA

  return(result)
}
