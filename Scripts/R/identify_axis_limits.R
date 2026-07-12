
identify_axis_limits <- function(results) {
  results |>
    filter_out(is.na(tidy)) |>
    unnest(tidy) |>
    summarize(.by = term,
              min_est = min(estimate, na.rm = T),
              max_est = max(estimate, na.rm = T))
}

# tar_read(results_grouped) |>
#   filter(condition_id == 1) |>
#   unnest(tidy)  |>
#   select(term) |>
#   unique()
