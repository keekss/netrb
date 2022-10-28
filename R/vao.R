vao <- function(sim, .attr) {

  if (.attr == 'rand') return(sim$vao_rand)
  else {
    vp <- vao_path(sim, .attr)

    # The simulator will always be set up with
    # random VAO determined.
    if (file.exists(vp)) return(fread(vp))
    else {
      # ticf('Calculating VAO for `%s` and writing to file...', .attr)
      g <- sim$g_orig
      vertex_attr_vals <- switch(
        .attr,
        'degr' = centr_degree(g)$res,
        'pgrk' = page_rank(g)$vector,
        # TODO change
        'clos' = igraph::centr_clo(g)$res,
        'betw' = centr_betw(g)$res,
        'eign' = centr_eigen(g)$vector,
        stop('Invalid attribute: ', .attr)
      )
      # Order vertices by decreasing value of attribute.
      # There may be many ties (e.g. 25% of vertices
      # in the power grid graph have degree of 1),
      # so settle ties randomly, since vertex ID
      # may not be related to any meaningful vertex properties.
      # tiebreaker <- sample()
      result <- as.integer(
        order(vertex_attr_vals,
              # Use standardized tiebreaker.
              sim$tiebreaker,
              decreasing = TRUE)
      )
      ensure_dir_exists(sprintf('%s/vaos', sim$root_dir))
      # fwrite(result, file = vp)
      # toc()
      return(result)
    }
  }
}


