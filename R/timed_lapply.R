timed_lapply <- function(x, func, ...,
                         times_run = 1L,
                         ncores    = NA) {
  if (is.na(ncores)) ncores <- detectCores() - 1

  result <- matrix(NA, nrow = length(x), ncol = 3)
  colnames(result) <- c('x', 'y', 't')
  result[, 'x'] <- x

  y_and_t_for_x_i <- function(x_i, func, ..., times_run = 1L) {
    mb <- microbenchmark(
      y_i <- func(x_i, ...),
      times = round(times_run),
      unit  = 's'
    )
    t_i <- summary(mb)$mean
    return(c(y_i, t_i))
  }
  y_and_t = mclapply(
    x, y_and_t_for_x_i, ...,
    func = func,
    times_run = times_run,
    mc.cores = ncores
  )
  # Get y and time values from each element in `y_and_t`.
  attr_from_each <- function(a, attr) {
    return(lapply(a, function(aelem) (aelem[[attr]])))
  }
  each_at_idx <- function(l, idx) {
    return(unlist(mclapply(
      l, function(l_i) {l_i[idx]}
    )))
  }
  result[, 'y'] <- each_at_idx(y_and_t, 1)
  result[, 't'] <- each_at_idx(y_and_t, 2)

  return(result)
}
