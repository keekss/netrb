update_seed <- function(rs) {
  if (!is.na(rs$seed_rand)) set.seed(rs$seed_rand)
}
