timed_lapply <- function(
  x, FUN, ...,
  times_run = 1L,
  ncores    = NA,
  randomize_order   = TRUE,
  return_times_only = FALSE,
  print_progress = FALSE
) {

  if (is.na(ncores)) ncores <- detectCores() - 1

  if (randomize_order) x <- sample(x)

  apply_fun <- if (print_progress) {
    pbmclapply
  } else mclapply

  if (return_times_only) {
    time_of_fun <- function(x_i) {
      mb <- microbenchmark(
        FUN(x_i, ...),
        times = times_run,
        unit  = 's'
      )
      return(summary(mb)$mean)
    }
    result <- unlist(apply_fun(
      x, time_of_fun,
      mc.cores  = ncores
    ))
    if (randomize_order) {
      result <- result[order(x)]
    }
    return(result)

  } else {
    result <- matrix(NA, nrow = length(x), ncol = 3)
    colnames(result) <- c('x', 'y', 't')
    result[, 'x'] <- x

    y_and_t_for_x_i <- function(x_i) {
      y_i_total <- 0
      mb <- microbenchmark(
        y_i_total <- (y_i_total + FUN(x_i, ...)),
        times = times_run,
        unit  = 's'
      )
      y_i <- y_i_total / times_run
      t_i <- summary(mb)$mean
      # Return as vector instead of list for performance
      return(c(y_i, t_i))
    }
    y_and_t = apply_fun(
      x, y_and_t_for_x_i,
      mc.cores  = ncores
    )
    # Get y and time values from each element in `y_and_t`.
    each_at_idx <- function(l, idx) {
      return(unlist(mclapply(
        l, function(l_i) {l_i[idx]}
      )))
    }
    result[, 'y'] <- each_at_idx(y_and_t, 1)
    result[, 't'] <- each_at_idx(y_and_t, 2)

    if (randomize_order) {
      result <- result[order(result[,'x']) ,]
    }
    return(result)
  }
}
