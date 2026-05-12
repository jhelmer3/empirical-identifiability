make_plt_dat <- function(bootres) {
  axis_label_width <- 5
  
  bootres |> pull(tidy) |> pluck(1) |> pull(term) |> unique() |>
    bs_plt_layout() |>
    mutate(plots = map(plot_terms,
                       \(plot_terms) plot_terms |>
                         mutate(plot = map2(xterm, yterm,
                                            \(xterm, yterm) {
                                              df <- bootres |>
                                                pull(tidy) |>
                                                list_rbind() |>
                                                filter(term %in% c(xterm, yterm)) |>
                                                select(term, estimate, iter) |>
                                                pivot_wider(names_from = term, values_from = estimate)
                                              
                                              mapping <- if (xterm == yterm) {
                                                do.call(aes, list(x = as.name(xterm)))
                                              } else {
                                                do.call(aes, list(y = as.name(xterm), x = as.name(yterm)))
                                              }
                                              geom <- if (xterm == yterm) geom_density() else geom_point(shape = 16,
                                                                                                         alpha = 0.3)
                                              
                                              ggplot(df, mapping) + 
                                                geom +
                                                scale_x_continuous(breaks = NULL) +
                                                scale_y_continuous(breaks = NULL) +
                                                labs(
                                                  x = if (xterm == yterm) wrap_axis_labels(xterm) else 
                                                    wrap_axis_labels(yterm),
                                                  y = if (xterm == yterm) NULL else 
                                                    wrap_axis_labels(xterm)
                                                ) +
                                                theme_classic()
                                            }))  |> pluck("plot")))
}
