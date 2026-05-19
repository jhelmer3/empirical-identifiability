
make_scatter_plt <- function(plt_dat, xterm, yterm, xlims = NULL, ylims = NULL) {
  
  ggplot(plt_dat, aes(x = .data[[xterm]], y = .data[[yterm]],
                      color = factor(error_type))) +
    geom_point(shape = 16, alpha = 0.3) +
    coord_cartesian(xlim = xlims, ylim = ylims) +
        scale_color_manual(values = c("0" = "black",
                                      "1" = "orange",
                                      "2" = "red",
                                      "3" = "purple")) +
        labs(x = wrap_axis_labels(yterm),
             y = wrap_axis_labels(xterm))
}