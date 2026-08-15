
init_params <- function(n_bootstraps, models, full_data = F,
                        full_and_cleaned_sample_sizes = NULL) {
  if (full_data) {
    ecls_cleaned_n_schools <- full_and_cleaned_sample_sizes |>
      filter(data_type == "cleaned") |>
      pull(n_schools)
    ecls_cleaned_n_students_per_school <- (
      (full_and_cleaned_sample_sizes |>
         filter(data_type == "cleaned") |>
         pull(n_students)) / ecls_cleaned_n_schools
    ) |>
      floor()
    
    n_students_set <- ecls_cleaned_n_students_per_school
    n_schools_set <- ecls_cleaned_n_schools
  }

  list(
    n_bootstraps =  n_bootstraps,
    full_data = full_data,
    n_students = if (full_data) n_students_set else c(3, 10),
    n_schools = if (full_data) n_schools_set else c(10, 30),
    model_string = models,
    centered = c(T, F),
    bootstrap_type = c("both", "cluster", "individual")
  )
}

# ((tar_read(full_and_cleaned_sample_sizes) |>
#   filter(data_type == "cleaned") |>
#   pull(n_observations)) / 841) |> floor()
