vdo <- function(
  del_attr, # Vertex attribute
  sim    = NULL,
  graph  = NULL,
  recalc = FALSE
) {

  if (!recalc && is.null(sim)) stop(
    'Must define a simulator to reuse VDOs.'
  )

  if (recalc) {
    g <- graph
    tiebreaker <- sample(1:vcount(g), vcount(g))

  } else {
    # If not recalculating, a simulator must have been passed.
    vp <- vdo_path(sim, del_attr)
    if (del_attr == 'random') return(sim$vdo_rand)
    else if (file.exists(vp)) return(fread(vp))
    else {
      g <- sim$g_orig
      tiebreaker <- sim$tiebreaker
    }
  }

  if (!recalc) ticf(
    'Calculating VDO for `%s` and writing to file...',
    va
  )

  vertex_attr_vals <- as.numeric(switch(
    del_attr,
    'random'   = sample(1:vcount(g), vcount(g)),
    'degree'   = degree(g),
    'pagerank' = page_rank(g)$vector,
    'harmonic' = harmonic_centrality(g),
    'close'    = centr_clo(g)$res,
    'between'  = centr_betw(g)$res,
    'eigen'    = centr_eigen(g)$vector,
    stop('Invalid attribute: ', del_attr)
  ))

  # Order vertices by decreasing value of attribute.
  # There may be many ties (e.g. 25% of vertices
  # in the power grid graph have degree of 1),
  # so settle ties randomly, since vertex ID
  # may not be related to any meaningful vertex properties.
  # tiebreaker <- sample()
  result <- as.integer(
    order(vertex_attr_vals,
          tiebreaker,
          decreasing = TRUE)
  )

  if (!recalc) {
    ensure_dir_exists(sprintf('%s/vdos', sim$root_dir))
    # fwrite(result, file = vp)
    toc()
  }

  return(result)
}




