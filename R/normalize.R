normalize <- function(arr,
                      min_init  = min(arr),
                      max_init  = max(arr),
                      min_final = 0,
                      max_final = 1) {

  normalize_val <- function(val) {
    scale <- (max_final - min_final) / (max_init - min_init)
    return(min_final + scale * (val - min_init))
  }

  return(sapply(arr, normalize_val))
}
