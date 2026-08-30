
patch_plt_dat <- function(condition_dat, axis_limits, full_data_models, table_height_prop = 1/4,
                          title_info_vars = c("n_students", "n_schools", "full_data")) {
  # mapping over `condition_id` in pipeline, so grabbing actual
  condition_id <- condition_dat$condition_id[[1]]
  # `model_string` is constant within `condition_id`
  condition_model_string <- condition_dat$model_string[[1]]
  
  # getting `plt_layout` and `true_values` for the condition's model
  plt_layout <- full_data_models |> 
    filter(model_string == condition_model_string) |> 
    pluck("plt_layout", 1)
  true_values <- full_data_models |> 
    filter(model_string == condition_model_string) |> 
    pluck("full_tidy", 1)
  
  condition_plt_dat <- make_plt_dat(condition_dat, plt_layout, true_values, axis_limits)
  
  n_terms <- plt_layout |>
    pull(n_terms) |>
    max()
  
  (condition_plt_dat |>
      pull(plots) |>
      unlist(recursive = F) |>
      append(wrap_table(make_warnings_tbl(condition_dat))) |>
      wrap_plots(design = plt_layout |> make_plt_pattern(),
                 axes = "collect",
                 heights = c(rep(((1 - table_height_prop) / n_terms), 
                                 times = n_terms),
                             table_height_prop))) |>
    cowplot::ggdraw() + 
    cowplot::draw_label(
      plot_annotation(condition_dat |> 
                        head(1) |>
                        select(all_of(title_info_vars)) |> 
                        imap_chr(\(value, name) glue::glue("{name}: {value}")) |> 
                        glue::glue_collapse(sep = ", ")),
      x = 0.5, y = 1, vjust = 1, fontface = "bold", size = 14)
}

# d <- tar_read(results_grouped_full_data) |>
#   first()
# # # issue: why is it 3 students 10 schools
# d |>
#   patch_plt_dat(tar_read(axis_limits_full_data))
#   head(1) |>
#   select(all_of(c("n_bootstraps", "n_students", "n_schools")))

# map(1:40, \(idx) {
#   print(idx)
#   
#   tar_read(results_grouped) |>
#     filter(condition_id == idx) |>
#     patch_plt_dat(tar_read(axis_limits))
# })
# 
# tar_read(results_grouped) |>
#   filter(condition_id == 9) |>
#   patch_plt_dat(tar_read(axis_limits))

# tar_read(results_grouped_v_subset) |>
#   filter(condition_id == 1) |> 
#   patch_plt_dat(tar_read(plt_layout),
#                 tar_read(true_values),
#                 tar_read(axis_limits_v_subset))
# 
# tar_read(results_grouped_v_subset) |>
#   filter(condition_id == 1) |> 
#   make_plt_dat(tar_read(plt_layout),
#                 tar_read(true_values),
#                 tar_read(axis_limits_v_subset)) |>
#   pull(plots)

# tar_read(params_w_subset) |>
#   pull(data_subset) |>
#   map(\(data) data$private |> sum())


# make_plt_dat(tar_read(examples_results_grouped) |>
#                filter(example_type == "bad"),
#              tar_read(plt_layout),
#              tar_read(examples_axis_limits))


# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   patch_plt_dat(tar_read(plt_layout), tar_read(true_values), tar_read(axis_limits))

# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   make_plt_dat(tar_read(plt_layout)) |>
#   pull(plots) |>
#   unlist(recursive = F) |> 
#   append(wrap_table(make_warnings_tbl(tar_read(results_grouped) |>
#                                         # mimic grouping
#                                         filter(condition_id ==  1))))
