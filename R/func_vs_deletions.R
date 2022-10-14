func_vs_deletions <- function(sim,
                              func, ...,
                              attrs   = c('rand'),
                              del_min = 0,
                              del_max = 0.9,
                              nchunks = 20,
                              pass_graph  = TRUE,
                              reuse_dists = TRUE) {
  require_class('simulator', rs, 'rs')

  # TODO docu doesn't matter which order extra parameters are in,
  # as long as parameter name is specified.

  func_name <- as.character(deparse(substitute(func)))
  # If the function name does not contain '::',
  # its library was not specified, so search for it
  # in `netrb` then `igraph` if not in `netrb`.
  if (!(grepl('::', func_name, fixed = TRUE))) {
    func <- verify_function(func_name)
  }

  g <- rs$g_orig


  chunks <- attr_order_chunks(attrs)

}
