plot_cols_vs_one_col <- function(.data,
                               x_col_name   = 'del_frac',
                               x_axis_label = 'x',
                               y_axis_label = 'y') {

  df <- cbind(
    .data[,1],
    reshape2::melt(.data[,2:ncol(.data)])[-1]
  )
  colnames(df) <- c('x', 'y', 'value')

  result <- ggplot(
    data = df,
    aes(x = x,
        y = value,
        group = y,
        color = y)
  ) + geom_line()
  return(result)
}
