
make_icc_dat <- function(condition_dat) {
  condition_dat |>
    select(model) |>
    mutate(
      icc = map_dbl(model, \(model) {
        icc <- performance::icc(model) |>
          pluck("ICC_adjusted")
        ifelse(is.null(icc), NA, icc)
      })
    )
}
