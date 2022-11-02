#' Robustness Simulation: Metric vs. Vertex Deletions
#'
#' @param metric_fun
#' @param ...
#' @param sim
#' @param graph
#' @param del_attrs
#' @param del_min
#' @param del_max
#' @param nchunks
#' @param plot
#' @param seed_rand
#' @param recalc_VDOs
#' @param pass_graph
#' @param reuse_dists
#'
#' @return
#' @export
#'
#' @examples
metric_vs_del <- function(
  metric_fun, ...,
  sim   = NULL,
  graph = NULL,
  del_attrs = c(
    'random',
    'degree',
    'pagerank',
    'harmonic',
    'close',
    'between',
    'eigen'),
  del_min = 0,
  del_max = 0.5,
  nchunks = 10,
  plot    = TRUE,
  seed_rand   = NULL,
  recalc_VDOs = TRUE,
  pass_graph  = TRUE,
  reuse_dists = FALSE
) {

  g_orig <- sim_or_graph_arg('graph', sim = sim, graph = graph)

  del_attrs <- match.arg(del_attrs,
                         several.ok = TRUE)

  metric_fun_name <- as.character(
    deparse(substitute(metric_fun))
  )

  # If the function name contains '::', its library was already specified.
  # If it contains '{' or 'function', it is an anonymous function.
  # Otherwise, search `netrb` first, then `igraph` for the function.
  if (!(any(grepl('::', metric_fun_name, fixed = TRUE),
            grepl('{',  metric_fun_name, fixed = TRUE),
            grepl('function',  metric_fun_name, fixed = TRUE)))) {
    metric_fun <- match_metric_fun(metric_fun_name)
  }

  result <- matrix(data = NA,
                   nrow = length(del_attrs) + 1,
                   ncol = nchunks + 1)
  rownames(result) <- c('del_frac', del_attrs)

  .vcount <- vcount(g_orig)

  # Since VDOs use vertex ids from the original graph,
  # "deleting" them via a boolean array allows for
  # fewer comparisons, and for vertices to be added back.
  vid_is_active <- matrix(data = TRUE,
                          nrow = length(del_attrs),
                          ncol = .vcount)
  rownames(vid_is_active) <- del_attrs

  # NOTE: VDO recalculation is not yet fully supported.
  .vdos <- vdos(del_attrs,
                sim    = sim,
                graph  = graph,
                recalc = recalc_VDOs)

  vdo_chunk <- function(vdoi_start, vdoi_end) {
    .result <- matrix(data = .vdos[, vdoi_start:vdoi_end],
                      nrow = length(del_attrs),
                      ncol = del_vdoi_end - del_vdoi_start + 1)
    rownames(.result) <- del_attrs
    return(.result)
  }

  del_vdo_chunk <- function(chunk) {

    for (da in del_attrs) {

      vids_to_delete <- as.integer(chunk[da, ])
      vid_is_active[da, vids_to_delete] <- FALSE

    }
    return(vid_is_active)
  }

  del_fracs <- seq(from = del_min,
                   to   = del_max,
                   by   = (del_max - del_min) / nchunks)

  pb <- startpb(min = 0, max = nchunks)
  chunk_idx <- 1
  setpb(pb, chunk_idx)
  del_vdoi_start <- 1
  del_vdoi_end   <- 1
  if (del_min > 0) {

    del_vdoi_end   <- to_idx(size = del_min, vec_len = .vcount)
    .vdo_chunk     <- .vdos[, 1:del_vdoi_end]
    vid_is_active  <- del_vdo_chunk(.vdo_chunk)
    del_vdoi_start <- del_vdoi_end + 1
  }
  update_result <- function() {
    result['del_frac', chunk_idx] <- del_fracs[chunk_idx]
    for (a in del_attrs) {
      g <- induced_subgraph(graph = g_orig,
                            vids  = vid_is_active[a,])

      if (pass_graph) {
        result[a, chunk_idx] <- metric_fun(graph = g, ...)
      } else {
        result[a, chunk_idx] <- metric_fun(...)
      }

    }
    return(result)
  }
  result <- update_result()
  chunk_idx <- 2
  # Iterate chunk end indices
  for (cei in chunk_end_indices(vec_len = .vcount,
                                nchunks = nchunks,
                                start   = del_vdoi_start,
                                end     = del_max)) {
    del_vdoi_end <- cei
    chunk <- vdo_chunk(del_vdoi_start, del_vdoi_end)

    vid_is_active <- del_vdo_chunk(chunk)
    result <- update_result()

    del_vdoi_start <- del_vdoi_end + 1
    chunk_idx <- chunk_idx + 1
    setpb(pb, chunk_idx)
  }

  closepb(pb)

  # return(result)

  result <- t(result)

  if (plot) plot_Ys_vs_X(
    result,
    y_axis_label = metric_fun_name,
  )

  return(result)
}
