normalize_vec <- function(
  vec,
  min_init  = min(vec),
  max_init  = max(vec),
  min_final = 0,
  max_final = 1
) {

  normalize_val <- function(val) {
    scale <- (max_final - min_final) / (max_init - min_init)
    return(min_final + scale * (val - min_init))
  }

  return(sapply(vec, normalize_val))
}
