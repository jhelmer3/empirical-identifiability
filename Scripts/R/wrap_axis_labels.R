wrap_axis_labels <- function(x) {
  x |>
    janitor::make_clean_names() |>
    str_replace_all("_+", " ") |>
    str_wrap(width = 10)
}

# d <- tibble(n_bootstraps = 10,
#             n_students = 4,
#             n_schools = 10) |>
#   get_bootres()
# 
# d |>
#   head(1) |>
#   pull(tidy) |>
#   pluck(1) |>
#   pull(term) |>
#   wrap_labels()

