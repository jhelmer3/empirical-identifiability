
make_r2_dat <- function(condition_dat) {
  condition_dat |>
    select(condition_id, model_id, centered, 
           bootstrap_type, btwn_condition_id, r2s) |>
    unnest(r2s) |>
    unnest(r2s)
}

# quiet_r2 <- purrr::quietly(\(model) performance::r2(model))
# 
# d <- tar_read(results_grouped) |>
#   filter(condition_id == 2)
# d |> make_r2_dat()
