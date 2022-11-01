vdo <- function(sim, .attr) {

  if (.attr == 'rand') return(sim$vdo_rand)
  else {
    vp <- vdo_path(sim, .attr)

    # The simulator will always be set up with
    # random VDO determined.
    if (file.exists(vp)) return(fread(vp))
    else {
      # ticf('Calculating VDO for `%s` and writing to file...', .attr)
      g <- sim$g_orig
      vertex_attr_vals <- as.numeric(switch(
        .attr,
        'degree'   = degree(g),
        'pagerank' = page_rank(g)$vector,
        'harmonic' = harmonic_centrality(g),
        'close'    = centr_clo(g)$res,
        'between'  = centr_betw(g)$res,
        'eigen'    = centr_eigen(g)$vector,
        stop('Invalid attribute: ', .attr)
      ))
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
      ensure_dir_exists(sprintf('%s/vdos', sim$root_dir))
      # fwrite(result, file = vp)
      # toc()
      return(result)
    }
  }
}


