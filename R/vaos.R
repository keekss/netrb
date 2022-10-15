vaos <- function(sim, attrs,
                 return_current = FALSE) {

  # Check how many VAOs must be determined.
  attrs_without_vao_stored <- attrs[
    sapply(attrs, function(a) {
      return(!(file.exists(vao_path(sim, a))))
    })
  ]

  if (length(attrs_without_vao_stored > 0)) {
    logf('%d of %d VAOs must be calculated.',
         length(attrs_without_vao_stored),
         length(attrs))
  }
  result <- t(sapply(
    attrs,
    function(a) vao(sim, a)
  ))
  colnames(result) <- NULL
  return(result)


}
