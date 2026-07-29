
make_r2_dat <- function(condition_dat) {
  condition_dat |>
  select(condition_id, model_id, centered, btwn_condition_id, model) |>
    mutate(
      r2s = map(model, \(model) {
        
        r2s <- quiet_r2(model)
        
        r2_conditional <- r2s$result$R2_conditional
        r2_marginal <- r2s$result$R2_marginal
        
        tribble(~ r2_type, ~ r2_estimate,
                "conditional", ifelse(is.null(r2_conditional), NA, r2_conditional),
                "marginal", ifelse(is.null(r2_marginal), NA, r2_marginal))
      })
    ) |>
    unnest(r2s)
}

quiet_r2 <- purrr::quietly(\(model) performance::r2(model))
# 
# d <- tar_read(results_grouped) |>
#   filter(condition_id == 2)
# d |> make_r2_dat()
