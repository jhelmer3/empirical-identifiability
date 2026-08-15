
subset_dat <- function(ecls_dat, params) {
  params |>
    nest(conditions = -c(n_bootstraps, n_students, n_schools, full_data)) |>
    mutate(
      data_subset = pmap(
        list(n_bootstraps, n_students, n_schools, full_data),
        \(n_bootstraps, n_students, n_schools, full_data)
        if (full_data) {
          ecls_dat
          }
        else
          {
          ecls_dat |>
            filter(schoolid %in% sample(schoolid, n_schools)) |>
            filter(.by = schoolid,
                   childid %in% sample(childid, n_students))
        }
      )
    ) |>
    unnest(conditions) |>
    nest(conditions = -c(n_bootstraps, n_students, n_schools,
                         full_data, data_subset, centered)) |>
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

# tar_read(params) |> 
#   mutate(keep_full_data = ifelse(n_students == max(n_students) & n_schools == max(n_schools), T, F)) |>
#   nest(conditions = -c(n_bootstraps, n_students, n_schools, keep_full_data)) |>
#   mutate(
#     data_subset = pmap(
#       list(n_bootstraps, n_students, n_schools, keep_full_data), 
#       \(n_bootstraps, n_students, n_schools, keep_full_data) if (keep_full_data) {
#         tar_read(ecls_dat) 
#         }
#       else {
#         tar_read(ecls_dat) |>
#           filter(schoolid %in% sample(schoolid, n_schools)) |>
#           filter(.by = schoolid,
#                  childid %in% sample(childid, n_students))
#       }
#     )
#   ) |>
#   pull(data_subset)

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
