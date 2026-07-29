
make_icc_dat <- function(condition_dat) {
  condition_dat |>
    select(condition_id, model_id, centered, btwn_condition_id, model) |>
    mutate(
      icc = map_dbl(model, \(model) {
        icc <- quiet_icc(model)
        ifelse(is.null(icc$result), NA, icc$result)
      })
    )
}

quiet_icc <- purrr::quietly(\(model) performance::icc(model) |>
                       pluck("ICC_adjusted"))

# d <- tar_read(results_grouped) |>
#   filter(condition_id == 2) 
# d |> make_icc_dat()

