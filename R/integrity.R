integrity <- function(rs, attr, state) {
  g <- g_at_del_state(rs, attr, state)

  return(vcount(largest_component(g)) / vcount(rs$g_orig))

}
