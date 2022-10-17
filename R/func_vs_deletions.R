func_vs_deletions <- function(sim,
                              func, ...,
                              attrs   = c('rand'),
                              del_min = 0,
                              del_max = 0.9,
                              nchunks = 20,
                              pass_graph  = TRUE,
                              reuse_dists = TRUE) {
  require_class('simulator', sim, 'sim')

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

  # TODO docu why doing this
  vid_is_active <- matrix(data = TRUE,
                          nrow = length(attrs),
                          ncol = .vcount)
  rownames(vid_is_active) <- attrs

  .vaos <- vaos(sim, attrs)

  vao_chunk <- function(vaoi_start, vaoi_end) {
    .result <- matrix(data = .vaos[, vaoi_start:vaoi_end],
                      nrow = length(attrs),
                      ncol = del_vaoi_end - del_vaoi_start + 1)
    rownames(.result) <- attrs
    return(.result)
  }

  del_vao_chunk <- function(chunk) {
    # print(class(vao_chunk))
    for (a in attrs) {

      vids_to_delete <- as.integer(chunk[a, ])
      vid_is_active[a, vids_to_delete] <- FALSE

    }
    return(vid_is_active)
  }

  pb <- startpb(min = 0, max = nchunks)
  chunk_idx <- 1
  setpb(pb, chunk_idx)
  del_vaoi_start <- 1
  del_vaoi_end   <- 1
  if (del_min > 0) {
    del_vaoi_end <- to_idx(size = del_min, arr_len = .vcount)
    vao_chunk   <- .vaos[1:del_vaoi_end]
    vid_is_active <- del_vao_chunk(vao_chunk)
    del_vaoi_start <- del_vaoi_end + 1
  }
  update_result <- function() {
    result['del_frac', chunk_idx] <- (del_vaoi_end - 1)/.vcount
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
                                start   = del_vaoi_start,
                                end     = del_max)) {
    del_vaoi_end <- cei
    chunk <- vao_chunk(del_vaoi_start, del_vaoi_end)
    # print(as.integer(chunk))
    vid_is_active <- del_vao_chunk(chunk)
    result <- update_result()

    del_vaoi_start <- del_vaoi_end + 1
    chunk_idx <- chunk_idx + 1
    setpb(pb, chunk_idx)
  }
  # print(vid_is_active)
  closepb(pb)
  return(result)
}
