fun_vs_del <- function(
    FUN, ...,
    sim   = NULL,
    graph = NULL,
    del_attrs = c('random',
                  'page_rank',
                  'degree'   = centr_degree(g)$res,
                  'pagerank' = page_rank(g)$vector,
                  'harmonic' = harmonic_centrality(g),
                  'close'    = centr_clo(g)$res,
                  'between'  = centr_betw(g)$res,
                  'eigen'    = centr_eigen(g)$vector,
                  ''),
    del_min = 0,
    del_max = 0.5,
    nchunks = 20,
    pass_graph  = TRUE,
    reuse_dists = TRUE) {



  # require_class('simulator', sim, 'sim')

  # TODO allow to pass any graph, not just simulator.

  # TODO docu doesn't matter which order extra parameters are in,
  # as long as parameter name is specified.
  # g <- duplicate(sim$g_orig)

  func_name <- as.character(deparse(substitute(func)))
  # If the function name does not contain '::',
  # its library was not specified, so search for it
  # in `netrb` then `igraph` if not in `netrb`.
  if (!(grepl('::', func_name, fixed = TRUE))) {
    func <- verify_function(func_name)
  }

  result <- matrix(data = NA,
                   nrow = length(attrs) + 1,
                   ncol = nchunks + 1)
  rownames(result) <- c('del_frac', attrs)

  .vcount <- vcount(sim$g_orig)

  # Since VDOs use vertex ids from the original graph,
  # "deleting" them via a boolean array allows for
  # fewer comparisons, and for vertices to be added back.
  vid_is_active <- matrix(data = TRUE,
                          nrow = length(attrs),
                          ncol = .vcount)
  rownames(vid_is_active) <- attrs

  .vdos <- vdos(sim, attrs)

  vdo_chunk <- function(vdoi_start, vdoi_end) {
    .result <- matrix(data = .vdos[, vdoi_start:vdoi_end],
                      nrow = length(attrs),
                      ncol = del_vdoi_end - del_vdoi_start + 1)
    rownames(.result) <- attrs
    return(.result)
  }

  del_vdo_chunk <- function(chunk) {

    for (a in attrs) {

      vids_to_delete <- as.integer(chunk[a, ])
      vid_is_active[a, vids_to_delete] <- FALSE

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
    for (a in attrs) {
      g <- induced_subgraph(graph = sim$g_orig,
                            vids  = vid_is_active[a,])
      result[a, chunk_idx] <- func(g = g, ...)
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
