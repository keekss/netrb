plot_Ys_vs_X <- function(
  df,
  x_col_name   = 'del_frac',
  x_axis_label = 'Vertices Deleted (Fraction of Total)',
  y_axis_label = 'Y',
  title        = sprintf('%s vs. %s',
                         y_axis_label,
                         x_axis_label),
  legend_title = 'VDO Attribute',
  return_plot  = FALSE
) {

  x_col_idx <- match(
    x     = x_col_name,
    table = colnames(df)
  )

  if (ncol(df) == 2) {
    df <- as.data.frame(cbind(
      df[,  x_col_idx],
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

  theme_update(plot.title = element_text(hjust = 0.5))

  result <- if (ncol(df) < 3) ggplot(
    data = df,
    aes(x = x,
        y = value)
  ) else ggplot(
    data  = df,
    aes(x = x,
        y = value,
        group = y,
        color = y)
  )

  result <- result +
    geom_line() +
    labs(x = x_axis_label,
         y = y_axis_label,
         title = title,
         color = legend_title)

  print(result)

  if (return_plot) return(result)
}
