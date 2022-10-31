distances <- function(g   = NA,
                      sim = NA,
                      v   = NA,
                      to  = NA,
                      diameter    = NA,
                      unconn_dist = Inf,
                      zero_as_NA = TRUE) {

  # if (is.na(sim)) {
  #   if (is.na(g)) stop('Must specify graph or simulator.')
  #   else {
  #     if (unconn_dist == Inf) {
  #       print('Using default unconnected distance of `diameter(g) + 1`.')
  #       # TODO check if `g` only has one component
  #       if (is.na(diameter)) {
  #         print('Manually calculating diameter.  It is recommended to pre-calculate this for large graphs.')
  #         unconn_dist <- diameter(g) + 1
  #       }
  #     }
  #   }
  # } else {
  #   g <- sim$g_orig
  #   unconn_dist <- sim$unconn_dist
  # }
  #
  # if (is.na(v))  v  = V(g)
  # if (is.na(to)) to = V(g)


  # TODO for other del_states
  result <- igraph::distances(g,
                              v  = v,
                              to = to)

  result[result == Inf] <- unconn_dist

  if (zero_as_NA) result[result == 0] <- NA

  return(result)
}
