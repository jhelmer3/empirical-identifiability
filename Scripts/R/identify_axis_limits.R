
identify_axis_limits <- function(results) {
  results |>
    unnest(tidy) |>
    summarize(.by = term,
              min_est = min(estimate, na.rm = T),
              max_est = max(estimate, na.rm = T))
}
