distances <- function(sim = NA,
                      g   = NA,
                      v   = NA,
                      to  = NA,
                      diameter    = NA,
                      unconn_dist = Inf,
                      zero_as_NA = TRUE) {



  # TODO for other del_states
  result <- igraph::distances(g,
                              v  = v,
                              to = to)

  result[result == Inf] <- unconn_dist

  if (zero_as_NA) result[result == 0] <- NA

  return(result)
}
