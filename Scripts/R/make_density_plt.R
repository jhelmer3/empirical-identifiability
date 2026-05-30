
make_density_plt <- function(plt_dat, true_values,
                             xterm, yterm, xlims = NULL, ylims = NULL) {
  
  ggplot(plt_dat, aes(x = .data[[xterm]])) +
    geom_density() +
    coord_cartesian(xlim = xlims) +
    labs(x = wrap_axis_labels(xterm),
         y = NULL) +
    annotate("segment", x = true_values |>
               filter(term == xterm) |>
               pull(estimate), y = 0, yend = Inf)
}

# x_term <- "(Intercept)"
# y_term <- "(Intercept)"
# 
# tar_read(true_values)
# 
# tar_read(results_grouped) |>
#   filter(condition_id == 1) |>
#   format_plt_dat(xterm = x_term, yterm = x_term) |>
#   make_single_plt(xterm = x_term, yterm = x_term,
#                   tar_read(results_grouped) |>
#                     filter(condition_id == 1) |> identify_axis_limits()) 
# 
# x_term <- "ses"
# y_term <- "ses:private"
# 
# tar_read(results_grouped) |>
#   filter(condition_id == 1) |>
#   format_plt_dat(xterm = x_term, yterm = y_term) |>
#   make_single_plt(xterm = x_term, yterm = y_term,
#                   tar_read(results_grouped) |>
#                     filter(condition_id == 1) |> identify_axis_limits())
