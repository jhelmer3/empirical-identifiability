
make_single_plt <- function(plt_dat, true_values,
                            xterm, yterm, axis_limits) {
  xlims <- axis_limits |>
    filter(term == xterm) |>
    select(-term) |>
    reduce(c)
  ylims <- axis_limits |>
    filter(term == yterm) |>
    select(-term) |>
    reduce(c)
  
  print(paste(xterm, yterm))
  
  {if (xterm == yterm) make_density_plt(plt_dat, true_values, 
                                        xterm, yterm, xlims, ylims)
  else make_scatter_plt(plt_dat, true_values,
                        xterm, yterm, xlims, ylims)} +
    # scale_x_continuous(breaks = NULL) +
    # scale_y_continuous(breaks = NULL) +
    theme_classic() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90))
}

# tar_read(results) |>
#   filter(model_id == 2) |>
#   first() |>
#   pull(model)

# tar_read(results) |>
#   filter_out(is.na(tidy)) |>
#   summarize(.by = term,
#             min_est = min(estimate, na.rm = T),
#             max_est = max(estimate, na.rm = T))

# true_values <- (tar_read(results_grouped) |>
                  # filter(condition_id == 9))[1, "full_tidy"][[1]][[1]]

# # add something as an indicator of how many are removed?
# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  9) |>
#   # first() |>
#   # select(tidy) |>
#   # unnest(tidy)
#   format_plt_dat("(Intercept)", "ses") |>
#   make_single_plt(true_values,
#                   "(Intercept)", "ses",
#                   tar_read(axis_limits))
# 
# tar_read(axis_limits) |>
#   filter(term == "ses") |>
#   select(-term) |>
#   reduce(c)

# make_single_plt <- function(plt_dat, xterm, yterm, axis_limits) {
# 
#   xlims <- axis_limits |>
#     filter(term == xterm) |>
#     select(-term) |>
#     reduce(c)
#   ylims <- axis_limits |>
#     filter(term == yterm) |>
#     select(-term) |>
#     reduce(c)
# 
#   if (xterm == yterm) make_density_plt()
#   else make_scatter_plt()
# 
#   mapping <- if (xterm == yterm) {
#     do.call(aes, list(x = as.name(xterm)))
#   } else {
#     do.call(aes, list(y = as.name(xterm), x = as.name(yterm)))
#   }
#   geom <- if (xterm == yterm) geom_density() else geom_point(shape = 16,
#                                                              alpha = 0.3)
#   ggplot(plt_dat, mapping) +
#     {if (xterm == yterm) geom_density() else geom_point(data = plt_dat,
#                                                         aes(color = factor(error_type),
#                                                             shape = factor(error_type)),
#                                                         shape = 16,
#                                                         alpha = 0.3)} +
#     scale_x_continuous(breaks = NULL) +
#     scale_y_continuous(breaks = NULL) +
#     scale_color_manual(values = c("0" = "black",
#                                   "1" = "orange",
#                                   "2" = "red",
#                                   "3" = "purple")) +
#     labs(
#       x = if (xterm == yterm) wrap_axis_labels(xterm) else
#         wrap_axis_labels(yterm),
#       y = if (xterm == yterm) NULL else
#         wrap_axis_labels(xterm)
#     ) +
#     theme_classic() +
#     theme(legend.position = "none")
# }

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
