
make_r2_dat <- function(condition_dat) {
  condition_dat |>
  select(model) |>
    mutate(
      r2s = map(model, \(model) {
        
        r2s <- performance::r2(model)
        
        r2_conditional <- r2s$R2_conditional
        r2_marginal <- r2s$R2_marginal
        
        tribble(~ r2_type, ~ r2_estimate,
                "conditional", ifelse(is.null(r2_conditional), NA, r2_conditional),
                "marginal", ifelse(is.null(r2_marginal), NA, r2_marginal))
      })
    ) |>
    unnest(r2s)
}