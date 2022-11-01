vdos <- function(sim, attrs,
                 return_all = FALSE) {
  attrs <- as.list(attrs)

  # Check how many VDOs must be determined.
  attrs_without_vdo_stored <- attrs[
    sapply(attrs, function(a) {
      return(!(file.exists(vdo_path(sim, a))))
    })
  ]

  if (length(attrs_without_vdo_stored > 0)) {
    logf('%d of %d VDOs must be calculated.',
         length(attrs_without_vdo_stored),
         length(attrs))
  }
  result <- t(sapply(
    attrs,
    function(a) vdo(sim, a)
  ))
  colnames(result) <- NULL
  return(result)


}
