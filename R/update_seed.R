update_seed <- function(rs) {
  if (!is.na(rs$rand_seed)) set.seed(rs$rand_seed)
}
