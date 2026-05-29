
patch_plt_dat <- function(condition_dat, plt_layout, axis_limits, table_height_prop = 1/4,
                          title_info_vars = c("n_bootstraps", "n_students", "n_schools")) {
  
  condition_plt_dat <- make_plt_dat(condition_dat, plt_layout, axis_limits)
  
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

# make_plt_dat(tar_read(examples_results_grouped) |> 
#                filter(example_type == "bad"),
#              tar_read(plt_layout),
#              tar_read(examples_axis_limits))


# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   # patch_plt_dat(tar_read(plt_layout))

# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   make_plt_dat(tar_read(plt_layout)) |>
#   pull(plots) |>
#   unlist(recursive = F) |> 
#   append(wrap_table(make_warnings_tbl(tar_read(results_grouped) |>
#                                         # mimic grouping
#                                         filter(condition_id ==  1))))
