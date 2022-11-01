vdos <- function(del_attrs,
                 sim    = NULL,
                 graph  = NULL,
                 recalc = FALSE) {

  g <- sim_or_graph_arg('graph', sim = sim, graph = graph)

  result <- t(sapply(
    del_attrs,
    vdo,
    sim    = sim,
    graph  = g,
    recalc = recalc
  ))
  colnames(result) <- NULL
  return(result)
}



# If no simulator is passed, VDOs must be calculated.
# if (is.null(sim) || recalc) {
#
#
# # Otherwise, check which VDOs have been calculated.
# } else {
#   del_attrs <- as.list(del_attrs)
#
#   # Check how many VDOs must be determined.
#   attrs_without_vdo_stored <- del_attrs[
#     sapply(del_attrs, function(a) {
#       return(!(file.exists(vdo_path(sim, a))))
#     })
#   ]
#
#   if (length(attrs_without_vdo_stored > 0)) {
#     logf('%d of %d VDOs must be calculated.',
#          length(attrs_without_vdo_stored),
#          length(attrs))
#   }
