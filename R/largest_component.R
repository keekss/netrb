largest_component <- function(g) {
  comps <- components(g)
  result_id <- which(
    comps$csize == max(comps$csize)
  )[1] # If multiple components are the largest, arbitrarily choose one.
  result_vertex_ids <- V(g)[
    comps$membership == result_id
  ]
  return(induced_subgraph(g, result_vertex_ids))
}
