plot_func_vs_deletions <- function(fvd) {
  fvd_long <- melt(fvd, id = 'del_frac')
  # print(result_long)
  # percentage <- function(num) return (paste(round(100 * num, 2), '%', sep=''))
  # TODO include warning about potentially different scounts
  # title_s <- paste('Distances of Closest', scount, 'Stations')
  # title_v <- paste('Vertices Remaining: ',
  #                  vcount(self$g), '/', vcount(self$g_orig),
  #                  '(', percentage(vcount(self$g)/vcount(self$g_orig)),
  #                  ')', sep = '')
  # title <- paste(title_s, title_v, sep = '\n')
  plot(ggplot(fvd_long,
              aes(x = x,
                  y = value,
                  color = variable))
       + geom_line())
       # + ggtitle(title)
       # + theme(plot.title = element_text(hjust = 0.5)))

}
