plot_cols_vs_one_col <- function(
    df,
    x_col_name   = 'del_frac',
    x_axis_label = 'x',
    y_axis_label = 'y') {

  # df <- as.data.frame(df)
  x_col_idx <- match(
    x     = x_col_name,
    table = colnames(df)
  )

  if (ncol(df) == 2) {
    df <- as.data.frame(cbind(
      df[, x_col_idx],
      df[, -x_col_idx]
    ))
    colnames(df) <- c('x', 'value')

  } else {
    df <- cbind(
      df[, x_col_idx],
      reshape2::melt(df[, -x_col_idx])[, -1]
    )

    print(df)
    colnames(df) <- c('x', 'y', 'value')
  }

  result <- ggplot(
    data = df,
    aes(x = x,
        y = value)
  ) + geom_line()

  if (ncol(df) > 2) {
    result <- result + aes(
      group = y,
      color = y
    )

  }

  # TODO labels etc.

  return(result)
}
