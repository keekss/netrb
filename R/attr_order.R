attr_order <- function(sim, .attr) {

  # The simulator will always be set up with
  # random VAO determined.
  if (.attr == 'rand') return(
    fread(attr_order_path(sim, .attr))
  ) else {
    if (file.exists(attr_order_path(sim, .attr))) {
      return(fread(attr_order_path(sim, .attr)))
      if (!is.na(rand_seed)) set.seed(rand_seed)
      rand_vertex_order <- sample(1:vcount(g))
      fwrite(rand_vertex_order,
             attr_order_path(rs, 'rand'))
    }

    if (.attr == 'rand') result <- sim$tiebreaker
    else {
      g <- sim$g_orig

      attr_vals <- switch(
        .attr,
        'degr' = centr_degree(g)$res,
        'clos' = centr_clo(g)$res,
        'betw' = centr_betw(g)$res,
        'eign' = centr_eigen(g)$vector,
        stop('Invalid attribute: ', .attr)
      )
      # Find vertex ranks. Settle ties randomly,
      # since vertex ID is not necessarily related
      # to any meaningful vertex properties, and there may be many ties
      # (e.g. 25% of vertices in the power grid graph have degree of 1).
      result <- order(attr_vals,
                      sim$tiebreaker,
                      decreasing = TRUE)
    }
    return(as.list(result))
  }


}
