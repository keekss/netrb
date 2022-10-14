c_del_to_del_state <- function(rs, c_del) {

  c_total <- rs$c_total

  if (c_del >= c_total) stop(
    'Invalid number of chunks to delete: ', c_del
  )
  else if (c_del == 0) return('0000')
  else {
    result_decimal <- format(c_del/c_total,
                             nsmall = 4,
                             scientific = FALSE)
    return(substr(result_decimal,
                  nchar(result_decimal) - 3,
                  nchar(result_decimal)))

  }
}

