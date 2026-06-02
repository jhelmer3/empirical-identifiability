
subset_dat <- function(ecls_dat, params) {
  params |>
    mutate(data_subset = pmap(list(n_bootstraps, n_students, n_schools), 
                              \(n_bootstraps, n_students, n_schools) 
                              ecls_dat |>
                                filter(schoolid %in% sample(schoolid, n_schools)) |>
                                filter(.by = schoolid,
                                       childid %in% sample(childid, n_students))))
}


