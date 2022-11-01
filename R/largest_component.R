largest_component <- function(graph) {
  comps <- components(graph)
  result_id <- which(
    comps$csize == max(comps$csize)
  )[1] # If multiple components are the largest, arbitrarily choose one.
  result_vertex_ids <- V(graph)[
    comps$membership == result_id
  ]
  return(induced_subgraph(
    graph = graph,
    vids  = result_vertex_ids
  ))
}
