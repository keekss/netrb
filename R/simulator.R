#' Robustness Simulation Container
#'
#' A container for performing robustness analysis on a graph while preserving the original.
#'
#' @param g        `(igraph)`    : the graph to analyze.
#' @param name     `(character)` : a unique identifier (string) for `rb_state` methods.
#' @param diam     `(integer)`   : diameter of `g` if pre-calculated.  Pre-calculation is highly recommended, as `rb_graph` objects are instantiated fairly often in this package, and diameter calculation for connected graphs is `O(E)`.
#' @param unconn_dist `(integer)`   : if two vertices are unconnected, `igraph::distances()` will return `INF` by default.  To help with averages (instead of ignoring `INF` values or having an `INF` average), `unconn_dist` can be assigned a value to replace `INF`. The default value is the graph's `diameter + 1`, to remain unchanged even as diameter decreases due to vertex removal in `rb_tester` functions.
#' @param seed_rand `(integer)`  : random number generator seed passed to `base::set.seed()`.
#' @param vrb       `(integer)`  : level of verbosity for printing progress bars, etc.
#'
#' @examples
simulator <- function(
    graph,
    .name,
    nchunks     = 5,
    diam        = NA,
    unconn_dist = NA, # `diam` + 1
    seed_rand   = 1,
    vrb         = 1,
    from_scratch = FALSE,
    trim_to_largest_component = TRUE) {

  if (is.null(seed_rand)) logf('Not standardizing random seed.  For standardized results, pass a value for `seed_rand`.')
  else {
    if (seed_rand < 0) stop('Negative random seeds are reserve for distance reuse functions.')
    logf(
      'Setting random seed to %d.  Default is 1; to leave seed unspecified, pass `seed_rand` of `NULL`.',
      seed_rand)
    set.seed(seed_rand)
  }

  all_sims_dir <- sprintf('%s/%s', getwd(), 'simulators')
  ensure_dir_exists(all_sims_dir)

  root_dir <- sprintf('%s/%s', all_sims_dir, .name)
  ensure_dir_exists(root_dir, must_be_empty = from_scratch)

  # By default, if `g` has multiple components, trim it to be
  # only the largest component, since stats for distances, etc.
  # assume having started with one component.
  if (trim_to_largest_component) {
    comps <- components(g)
    if (comps$no > 1) {
      logf('Graph passed to `simulator()` has more than 1 component.  For analysis, trimming graph to largest component only.')
      g <- largest_component(g)
    }
  }

  vertex_attr(g, 'vid_orig') <- V(g)

  # TODO
  # g_orig <- duplicate(g)
  # lockBinding("g_orig", globalenv())

  # ensure_dir_exists(sprintf('%s/vdos', root_dir))
  # vdo_rand_path <- sprintf('%s/vdos/rand.csv', root_dir)
  # if (!(file.exists(vdo_rand_path))) {
  #   if (!is.na(seed_rand)) set.seed(seed_rand)
  #   rand_vertex_order <- as.list(sample(1:vcount(g)))
  #   fwrite(rand_vertex_order, file = vdo_rand_path)
  # }
  set.seed(seed_rand)
  vdo_rand   <- sample(1:vcount(g))
  set.seed(max(2, seed_rand + 1)) # 0 and 1 are the same
  tiebreaker <- sample(1:vcount(g))


  # Calculate diameter if not specified
  if (is.na(diam)) {
    ticf('Diameter not specified.  Manually calculating...')
    diam <- diameter(g)
    toc()
  }
  if (is.na(unconn_dist)) {
    logf('Using default unconnected distance of (`diameter` + 1).')
    unconn_dist <- diam + 1
  }

  # TODO write summary

  return(list(
    class      = 'simulator',
    # g          = g,
    # Copy `g` as a backup.
    g_orig     = g,#g_orig,
    vdo_rand   = vdo_rand,
    tiebreaker = tiebreaker,
    root_dir   = root_dir,
    .name      = .name,
    diam_orig       = diam,
    unconn_dist   = unconn_dist,
    seed_rand  = seed_rand,
    vrb        = vrb
  ))
}
