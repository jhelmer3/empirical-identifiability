
make_plt_dat <- function(condition_dat, plt_layout, true_values, axis_limits) {
  axis_label_width <- 5
  
  plt_layout |>
    mutate(plots = map(plot_terms,
                       \(plot_terms) plot_terms |>
                         mutate(plot = map2(xterm, yterm,
                                            \(xterm, yterm) {
                                              format_plt_dat(condition_dat, xterm, yterm) |>
                                              make_single_plt(true_values, xterm, yterm, axis_limits)
                                            })) |> pluck("plot")))
}

# tar_read(full_data_models)[2, "plt_layout"][[1]][[1]][2, "plot_terms"][[1]]
# 
# plt_layout <- (tar_read(results_grouped) |>
#                  filter(condition_id == 9))[1, "plt_layout"][[1]][[1]]
# true_values <- (tar_read(results_grouped) |>
#                   filter(condition_id == 9))[1, "full_tidy"][[1]][[1]]
# 
# pltdat <- tar_read(results_grouped) |>
#   filter(condition_id == 9) |>
#   make_plt_dat(plt_layout, true_values, tar_read(axis_limits))
# pltdat$plots
# 
# 
# 
# (pltdat |>
#     pull(plots) |>
#     unlist(recursive = F) |>
#     append(wrap_table(make_warnings_tbl(tar_read(results_grouped) |>
#                                           filter(condition_id == 1)))) |>
#     wrap_plots(design = plt_layout |> make_plt_pattern(),
#                # axes = "collect",
#                heights = c(rep(((1 - table_height_prop) / 3), 
#                                times = 3),
#                            table_height_prop)))
