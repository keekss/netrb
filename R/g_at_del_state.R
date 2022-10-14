g_at_del_state <- function(rs, attr, state) {

  g <- duplicate(rs$g_orig)
  if (state == 'full') return(g)
  else {
    vertex_attr(g, 'active') <- TRUE

    c_active <- del_state_to_c_del(rs, state)


    c_names_to_remove <- 1:(rs$c_total - c_active)

    ensure_chunks(rs = rs,
                  attr = attr)

    c_paths <- sapply(
      c_names_to_remove,
      function(c_name) {
        chunk_path(rs = rs,
                   attr = attr,
                   c_name = c_name)
      }
    )

    for (p in c_paths) {
      vertex_attr(g,
                  'active',
                  index = fread(p)) <- FALSE
    }

    return(induced.subgraph(
      g,
      vids = vertex_attr(g,
                         'active')))
  }


}
