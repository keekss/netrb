mean_distance_approx <- function(sim = NA, # TODO add sim stuff, standardize with distances function?
                                 g   = NA,
                                 N = NA,
                                 lpr = NA,
                                 fit = NA,
                                 pro = NA,
                                 vs1 = NA,
                                 vs2 = NA,
                                 times_run = 1) {
  foo10 <- function() {
    x <- rnorm(N, sample(lpr, size=N, replace=TRUE), fit$bw)
    x <- x - min(x)
    x <- x / max(x)
    x <- x * (vcount(g)-1) + 1
    return(unlist(pro[x]))
  }

  if (!is.na(N)) {
    vids1 <- foo10()
    vids2 <- foo10()
  } else {
    vids1 <- sample(1:vcount(g), to_idx(vs1, vcount(g)))
    vids2 <- sample(1:vcount(g), to_idx(vs2, vcount(g)))
  }

  result <- rep(NA, times_run)
  result <- unlist(mclapply(
    result,
    function(na) {
      mean(netrb::distances(sim = sim,
                            g   = g,
                            v   = vids1,
                            to  = vids2,
                            zero_as_NA = TRUE),
           na.rm = TRUE)
    },
    mc.cores = detectCores() - 1
  ))
  return(mean(result))
}
