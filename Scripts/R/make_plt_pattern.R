make_plt_pattern <- function(plt_dat) {
  plt_dat |>
    select(n_terms, n_prev_terms) |>
    mutate(areas = map(n_terms, 
                       \(n_terms) seq(1, n_terms) |>
                         map2(n_terms, \(y, x) c(x, y))
    )) |>
    pull(areas) |>
    list_flatten() |>
    map(\(a) area(t = a[1], l = a[2], b = a[1], r = a[2])) |>
    append(list(area(
      t = get_table_row_location(plt_dat), 
      l = 1, 
      b = get_table_row_location(plt_dat), 
      r = get_table_row_location(plt_dat) - 1
      ))) |>
    reduce(c)
}

get_table_row_location <- function(plt_dat) {
  (plt_dat |>
    pull(n_terms) |>
    max()) + 1
}

# d <- get_bootres(tibble(n_bootstraps = 10,
#                            n_students = 10,
#                            n_schools = 10,
#                            condition_id = 1))

# bres |>
#   make_plt_dat() |>
#   make_plt_pattern() |>
#   plot()