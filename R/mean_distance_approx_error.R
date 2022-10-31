mean_distance_approx_error <- function(true_mean_distance, ...) {
  approx <- netrb::mean_distance_approx(...)
  return(abs(approx - true_mean_distance) / true_mean_distance)
}
