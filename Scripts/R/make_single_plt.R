
make_single_plt <- function(plt_dat, xterm, yterm) {
  mapping <- if (xterm == yterm) {
    do.call(aes, list(x = as.name(xterm)))
  } else {
    do.call(aes, list(y = as.name(xterm), x = as.name(yterm)))
  }
  geom <- if (xterm == yterm) geom_density() else geom_point(shape = 16,
                                                             alpha = 0.3)
  ggplot(plt_dat, mapping) + 
    {if (xterm == yterm) geom_density() else geom_point(data = plt_dat,
                                                        aes(color = factor(error_type),
                                                            shape = factor(error_type)),
                                                        shape = 16,
                                                        alpha = 0.3)} +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    scale_color_manual(values = c("0" = "black",
                                  "1" = "orange",
                                  "2" = "red",
                                  "3" = "purple")) +
    labs(
      x = if (xterm == yterm) wrap_axis_labels(xterm) else 
        wrap_axis_labels(yterm),
      y = if (xterm == yterm) NULL else 
        wrap_axis_labels(xterm)
    ) +
    theme_classic() +
    theme(legend.position = "none")
}

# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   format_plt_dat("(Intercept)", "(Intercept)") |>
#   make_single_plt("(Intercept)", "(Intercept)")
# 
# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   format_plt_dat("(Intercept)", "ses") |> 
#   make_single_plt("(Intercept)", "ses")
# 
# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   format_plt_dat("ses:private", "ses") |>
#   make_single_plt("ses:private", "ses") 
