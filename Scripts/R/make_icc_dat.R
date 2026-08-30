
make_icc_dat <- function(condition_dat) {
  condition_dat |>
    select(condition_id, model_id, centered, 
           bootstrap_type, btwn_condition_id, icc)
}

# quiet_icc <- purrr::quietly(\(model) performance::icc(model) |>
#                        pluck("ICC_adjusted"))

# d <- tar_read(results_grouped) |>
#   filter(condition_id == 2) 
# d |> make_icc_dat()

