del_state_to_c_del <- function(rs, state) {

  result <- as.numeric(state) * rs$c_total / 1e4

  # Ensure result is an integer
  if (result != as.integer(result)) {
    stop('Invalid state: ', state)
  } else return(result)
}
