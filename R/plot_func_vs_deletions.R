plot_func_vs_deletions <- function(fvd, func_name) {
  df <- cbind(
    fvd[,1],
    reshape2::melt(fvd[,2:ncol(fvd)])[-1]
  )
  colnames(df) <- c('del_frac', 'attr', 'value')

  result <- ggplot(
    data = df,
    aes(x = del_frac,
        y = value,
        group = attr,
        color = attr)
  ) + geom_line()
  return(result)
}
