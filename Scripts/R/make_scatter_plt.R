
make_scatter_plt <- function(plt_dat, true_values,
                             xterm, yterm, xlims = NULL, ylims = NULL) {
  
  ggplot(plt_dat, aes(x = .data[[xterm]], y = .data[[yterm]],
                      color = factor(error_type))) +
    geom_point(shape = 16, alpha = 0.3) +
    annotate("segment", x = true_values |>
               filter(term == xterm) |>
               pull(estimate), 
             y = -Inf,
             yend = true_values |>
               filter(term == yterm) |>
               pull(estimate)) +
    annotate("segment", y = true_values |>
               filter(term == yterm) |>
               pull(estimate), 
             x = -Inf,
             xend = true_values |>
               filter(term == xterm) |>
               pull(estimate)) +
    coord_cartesian(xlim = xlims, ylim = ylims) +
        scale_color_manual(values = c("0" = "black",
                                      "1" = "orange",
                                      "2" = "red",
                                      "3" = "purple")) +
        labs(x = wrap_axis_labels(yterm),
             y = wrap_axis_labels(xterm))
}
