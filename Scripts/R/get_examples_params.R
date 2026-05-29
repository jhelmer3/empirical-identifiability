
get_examples_params <- function(examples_dat, n_bootstraps) {
  examples_dat |>
  unnest(tidy) |>
  select(example_type, n_bootstraps, n_schools, n_students, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  uncount(n_bootstraps, .id = "iter", .remove = F)
}

# get_examples_params(tar_read(examples_dat), 
#                     tar_read(n_bootstraps))
# tar_read(examples_params)
