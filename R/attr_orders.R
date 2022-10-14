attr_orders <- function(rs, attrs) {

  # Check how many orders must be determined.


  if (attr == 'rand') return(rs$tiebreaker)
  else {
    g <- rs$g_orig

    attr_vals <- switch(
      attr,
      'degr' = centr_degree(g)$res,
      'clos' = centr_clo(g)$res,
      'betw' = centr_betw(g)$res,
      'eign' = centr_eigen(g)$vector,
      stop('Invalid attribute: ', attr)
    )
    # Find vertex ranks. Settle ties randomly,
    # since vertex ID is not necessarily related
    # to any meaningful vertex properties, and there may be many ties
    # (e.g. 25% of vertices in the power grid graph have degree of 1).
    return(order(attr_vals,
                 rs$tiebreaker,
                 decreasing = TRUE))
  }
}
