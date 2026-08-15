
get_full_and_cleaned_sample_sizes <- function(ecls_full, ecls_cleaned) {
  list("full" = ecls_full |> rename(schoolid = s2_id), 
       "cleaned" = ecls_cleaned) |>
    imap(
      \(data, idx) data |>
        summarize(
          data_type = idx,
          n_students = length(unique(childid)),
          n_schools = length(unique(schoolid)),
          n_observations = n()
        )
    ) |>
    list_rbind()
}

# ecls_full <- tar_read(ecls)
# ecls_subset <- tar_read(ecls_dat)

# get_full_and_subset_sample_sizes(ecls_full, ecls_subset)


