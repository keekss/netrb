attr_orders <- function(rs, attrs,

                        # TODO
                        return_current = FALSE) {


  # Check how many orders must be determined.
  # .dir <- attr_orders_dir(rs)
  # ensure_dir_exists(.dir)

  attrs_not_stored <- attrs[sapply(attrs, function(a) {
    return(!(file.exists(attr_order_path(rs, a))))
  })]

  if (length(attrs_not_stored > 0)) {
    logf('%d of %d attribute orders: calculating and writing...',
         length(attrs_not_stored),
         length(attrs))
    for (a in attrs_not_stored) {
      # logf('ao %s', attr_order(a))
      fwrite(attr_order(a), attr_order_path(rs, a))
    }
  }
  # TODO make it so that only the ones that need to be written are re-read afterward...
  # can make it so each one is either read in or calculated...
  result <- t(sapply(attrs, function(a) {
    return(fread(attr_order_path(rs, a)))
  }))
  colnames(result) <- NULL
  # names(result) <- attrs

  # print(result)
  # return(as.list(result))
  return(result)


}
