centr_clo_top_approx <- function(
  sim,
  top_sample_size,
  page_rank_sample_multiplier,
  distance_sample_frac
){



  # TODO have different deletion levels
  g <- sim$g_orig

  vcount_top <- to_idx(size = top_sample_size,
                       arr_len = vcount(g))
  vcount_pgrk <- round(vcount_top * page_rank_sample_multiplier)
  vcount_dist <- to_idx(size = distance_sample_frac,
                        arr_len = vcount(g))

  vao_pgrk_all <- order(log(page_rank(sim$g_orig)$vector),
                        sim$tiebreaker,
                        decreasing = TRUE)
  vao_pgrk <- vao_pgrk_all[1:vcount_pgrk]
  vao_dist_idx <- unique(round(seq(from = 1,
                            to   = min(10 * vcount_pgrk, vcount(g)),
                            length.out = vcount_dist)))
  vids_dist <- vao_pgrk_all[vao_dist_idx]
  # print('va')
  # print(vao_pgrk_all)
  # print('vp')
  # print(vao_pgrk)
  # print('vd')
  # print(vids_dist)

  # print(vao_pgrk)
  # pgrks <- page_rank(g, vids = vao_pgrk)$vector
  prr <- 1:vcount_pgrk
  # print(prr)
  # print(max(pgrks))
  # print(min(pgrks))


  update_seed_rand(sim)
  # vids_dist <- sample(vertex_attr(g,
  #                                 'vid_orig'),
  #                     size = vcount_dist)
  # vids_dist <- sort(vids_dist)
  # length(intersect(vao_pgrk, vids_dist))
  # print(vids_dist)
  # print(class(vids_dist))
  tic('dists')
  dists <- distances(sim,
                     v = vao_pgrk,
                     to = vids_dist)
  toc()
  print(dim(dists))

  colnames(dists) <- vids_dist
  rownames(dists) <- vao_pgrk

  dists_total <- apply(dists, 1, sum)
  # avg_clos  <- 1/avg_dists
  # print('ac_val')
  # print(avg_clos)
  ac_ranks <- rank(dists_total, ties.method = 'first')
  # print(ac_ranks)
  # print('z')
  # print(length(ac_ranks))
  # print(max(avg_clos))
  # print(min(avg_clos))
  res <- (1 * prr + 1 * ac_ranks)
  # print('x')
  # print(res)
  # print(max(res))
  # print(min(res))

  res_o <- order(res)[1:vcount_top]

  res_vids <- vao_pgrk[res_o]
  # print(res_vids)
  return(res_vids)
}
