dists_path <- function(rs, attr, state) {
  return(sprintf('%s/%s.csv',
                 dists_dir(rs = rs, attr = attr),
                 state))
}
