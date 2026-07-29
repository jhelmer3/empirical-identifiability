
subset_dat <- function(ecls_dat, params) {
  params |>
    nest(conditions = -c(n_bootstraps, n_students, n_schools)) |>
    mutate(
      data_subset = pmap(
        list(n_bootstraps, n_students, n_schools), 
        \(n_bootstraps, n_students, n_schools) {
          ecls_dat |>
            filter(schoolid %in% sample(schoolid, n_schools)) |>
            filter(.by = schoolid,
                   childid %in% sample(childid, n_students))
        }
      )
    ) |>
    unnest(conditions) |>
    nest(conditions = -c(n_bootstraps, n_students, n_schools, 
                         data_subset, centered)) |>
    mutate(data_subset = map2(
      centered, data_subset,
      \(centered, data_subset) if (centered) {
        data_subset |>
          mutate(rural = rural - mean(rural),
                 ses = ses - mean(ses))
      } else {
        data_subset
      }
    )) |>
    unnest(conditions)
}

# subset_dat(tar_read(ecls_dat), tar_read(params)) |>
  # select(data_subset) |>
  # unique()

# 
# tar_read(params) |>
#   first() |>
#   mutate(
#     data_subset = pmap(
#       list(n_bootstraps, n_students, n_schools, centered), 
#       \(n_bootstraps, n_students, n_schools, centered) 
#       tar_read(ecls_dat) |>
#         filter(schoolid %in% sample(schoolid, n_schools)) |>
#         filter(.by = schoolid,
#                childid %in% sample(childid, n_students)) |>
#         mutate(rural = map2_dbl(rural, centered,
#                                 \(rural, centered) ifelse(centered,
#                                                           rural - mean(rural), rural)),
#                ses = map2_dbl(ses, centered,
#                               \(ses, centered) ifelse(centered,
#                                                       ses - mean(ses), ses)))
#     )
#   ) |>
#   pluck("data_subset", 1) |>
#   summary()
# 
# tar_read(params_w_subset) |> 
#   slice(.by = centered, 1) |>
#   pull(data_subset) |>
#   map(~ summary(.x))
  

# tar_read(params) |>
#   slice_head(n = 1) |>
#   subset_dat(tar_read(ecls_dat), params = _) |>
#   pluck("data_subset", 1) |>
#   summary()

# subset <- tar_read(results) |>
#   select(data_subset) |>
#   slice(2)
# subset[[1]][[1]] |>
#   summary()
