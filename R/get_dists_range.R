get_dists_range <- function(rs,
                            attr,
                            max_del_state = '9000',
                            .write = TRUE) {

  # TODO Fix

  root_dir <- dists_dir(rs, attr)
  ensure_dir_exists(root_dir)

  states_all <- sapply(rs$c_total:c_active_min,
                       c_active_to_state)

  states_to_calc <- states_all[sapply(
    states_all, function(s) {
      p <- dists_path(rs    = rs,
                      attr  = attr,
                      state = s)
      return(!(file.exists(p)))
    }
  )]

  return(states_to_calc)


  # Check how many distances have not been written
  # paths <- dists_paths(rs = rs,
  #                      attr = attr,
  #                      all_c_active = all_c_active,
  #                      c_total = c_total)
  #


  # TODO cleanup
  # num_dists_to_return <- c_total - c_active_min + 1
  # num_dists_already_calculated <- sum(sapply(
  #   all_paths, file.exists
  # ))
  # num_dists_to_calculate <- num_dists_to_return - num_dists_already_calculated
  # if (num_dists_to_calculate > 0) print(sprintf(
  #   '%d of %d distances: calculating and writing...',
  #   num_dists_to_calculate,
  #   num_dists_to_return
  # ))
  #
  #
  # # Calculate and write distances needed, printing progress
  # pb <- startpb(min = 1, max = num_dists_to_calculate)
  # paths_to_calculate <- (all_paths[sapply(
  #   all_paths, function(p) (!(file.exists(p)))
  # )])
  # for (i in 1:length(paths_to_calculate)) {
  #
  # }
  # closepb(pb)
  #
  # return(0)
  #
  # # If distances have already been calculated for state, return them.
  # # search distances directory for state.
  # p_file_names <- all_p_active_strs(p_total, as_csv = TRUE)
  # p_file_paths <- paste(d_dir, p_file_names, sep = '/')
  # p_prev_calc  <- sapply(p_file_paths, file.exists)
  # p_need_calc  <- p_total - sum(p_prev_calc)
  # if (rs$vrb) log_info(sprintf(
  #   'Need to calculate distances for %d of %d partitions.',
  #   p_need_calc, p_total
  # ))
  # if (rs$vrb) tic('Calculating...')
  #
  #
  # if (rs$vrb) toc()



}
