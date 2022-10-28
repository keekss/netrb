update_seed_rand <- function(sim) {
  if (!is.na(sim$seed_rand)) set.seed(sim$seed_rand)
}
