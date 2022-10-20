vao <- function(sim, .attr) {

  vp <- vao_path(sim, .attr)

  # The simulator will always be set up with
  # random VAO determined.
  if (file.exists(vp)) return(fread(vp))
  else {
    ticf('Calculating VAO for `%s` and writing to file...', .attr)
    g <- sim$g_orig
    vertex_attr_vals <- switch(
      .attr,
      'degr' = centr_degree(g)$res,
      'clos' = centr_clo(g)$res,
      'betw' = centr_betw(g)$res,
      'eign' = centr_eigen(g)$vector,
      stop('Invalid attribute: ', .attr)
    )
    # Order vertices by decreasing value of attribute.
    # There may be many ties (e.g. 25% of vertices
    # in the power grid graph have degree of 1),
    # so settle ties randomly, since vertex ID
    # may not be related to any meaningful vertex properties.
    set.seed(2)
    # tiebreaker <- sample()
    result <- as.list(
      order(vertex_attr_vals,
            # Use standardized random vertex order as tiebreaker.
            as.integer(fread(vao_path(sim, 'rand'))),
            decreasing = TRUE)
    )
    fwrite(result, file = vp)
    toc()
    return(result)
  }
}


