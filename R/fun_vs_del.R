fun_vs_del <- function(
    FUN, ...,
    sim   = NULL,
    graph = NULL,
    del_attrs = c('random',
                  'page_rank',
                  'degree',
                  'pagerank',
                  'harmonic',
                  'close',
                  'between',
                  'eigen'),
    del_min = 0,
    del_max = 0.5,
    nchunks = 20,
    seed_rand   = NULL,
    recalc_VDOs = TRUE,
    pass_graph  = TRUE,
    reuse_dists = TRUE) {


  g      <- sim_or_graph_arg('graph',  sim = sim, graph = graph)
  g_orig <- sim_or_graph_arg('g_orig', sim = sim, graph = graph)

  del_attrs <- match.arg(del_attrs,
                         several.ok = TRUE)

  fun_name <- as.character(deparse(substitute(FUN)))
  # If the function name does not contain '::',
  # its library was not specified, so search for it
  # in `netrb` then `igraph` if not in `netrb`.
  if (!(grepl('::', fun_name, fixed = TRUE))) {
    FUN <- match_metric_fun(fun_name)
  }

  result <- matrix(data = NA,
                   nrow = length(del_attrs) + 1,
                   ncol = nchunks + 1)
  rownames(result) <- c('del_frac', del_attrs)

  # TODO !!! if recalculating,
  # make sure the graph is updated if a simulator is used
  # TODO vdo() will not check.

  .vcount <- vcount(g)

  # Since VDOs use vertex ids from the original graph,
  # "deleting" them via a boolean array allows for
  # fewer comparisons, and for vertices to be added back.
  vid_is_active <- matrix(data = TRUE,
                          nrow = length(del_attrs),
                          ncol = .vcount)
  rownames(vid_is_active) <- del_attrs

  .vdos <- vdos(del_attrs,
                sim = sim,
                graph = graph,
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
    del_vdoi_end <- to_idx(size = del_min, arr_len = .vcount)
    vdo_chunk   <- .vdos[1:del_vdoi_end]
    vid_is_active <- del_vdo_chunk(vdo_chunk)
    del_vdoi_start <- del_vdoi_end + 1
  }
  update_result <- function() {
    result['del_frac', chunk_idx] <- del_fracs[chunk_idx]
    for (a in del_attrs) {
      g <- induced_subgraph(graph = g_orig,
                            vids  = vid_is_active[a,])
      result[a, chunk_idx] <- FUN(g = g, ...)
    }
    return(result)
  }
  result <- update_result()
  chunk_idx <- 2
  for (cei in chunk_end_indices(arr_len = .vcount,
                                nchunks = nchunks,
                                start   = del_vdoi_start,
                                end     = del_max)) {
    del_vdoi_end <- cei
    chunk <- vdo_chunk(del_vdoi_start, del_vdoi_end)
    # print(as.integer(chunk))
    vid_is_active <- del_vdo_chunk(chunk)
    result <- update_result()

    del_vdoi_start <- del_vdoi_end + 1
    chunk_idx <- chunk_idx + 1
    setpb(pb, chunk_idx)
  }
  # print(vid_is_active)
  closepb(pb)
  return(t(result))
}
