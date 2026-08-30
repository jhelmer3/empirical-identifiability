
fit_model <- function(bootstraps) {
  
  fit_one <- function(data, model_string) {
    result <- safely(\(data) lme4::lmer(as.character(model_string), data), otherwise = NA_real_)(data)
    model <- result$result
    error <- pluck(result, "error", "message", .default = NA_character_)
    
    tidy_out <- if (is.na(error)) broom.mixed::tidy(model) else NA
    opt_warn <- get_warnings(model)
    lme4_warn <- get_lme4_warnings(model)
    
    icc_result <- quiet_icc(model)
    icc <- ifelse(is.null(icc_result$result), NA, icc_result$result)
    
    r2s_result <- quiet_r2(model)
    r2_conditional <- r2s_result$result$R2_conditional
    r2_marginal <- r2s_result$result$R2_marginal
    # next time we do a big run, try to keep the r2s nested by changing `fit_model()`
    r2s <- tribble(~ r2_type, ~ r2_estimate,
            "conditional", ifelse(is.null(r2_conditional), NA, r2_conditional),
            "marginal", ifelse(is.null(r2_marginal), NA, r2_marginal))
    
    rm(model, result, icc_result, r2s_result, r2_conditional, r2_marginal) 
    gc()
    
    tibble(error = error, tidy = list(tidy_out), 
           opt_warnings = list(opt_warn), lme4_warnings = list(lme4_warn),
           icc = icc, r2s = r2s)
  }
  
  bootstraps |>
    mutate(fit_results = map2(bootstrap, model_string, fit_one)) |>
    unnest(fit_results) |>
    select(-bootstrap)
}

quiet_icc <- purrr::quietly(\(model) performance::icc(model) |>
                              pluck("ICC_adjusted"))

quiet_r2 <- purrr::quietly(\(model) performance::r2(model))


# tar_read(params_w_subset_subset) |>
#   first() |>
#   bootstrap_subset() |>
#   fit_model()

# fit_model <- function(bootstraps) {
#   
#   safe_specific_model <- function(data, model) {
#     safely(
#       \(data) lme4::lmer(model, data),
#       otherwise = NA_real_
#     )(data)
#   }
#   
#   bootstraps |>
#     mutate(res = map2(bootstrap, model_string, 
#                       \(bootstrap, model_string) safe_specific_model(bootstrap, model_string)),
#            model = map(res, "result"),
#            error = map_chr(res, \(res) pluck(res, "error", "message", .default = NA_character_)),
#            tidy = map2(model, error, \(model, error) 
#                       if (is.na(error)) broom.mixed::tidy(model)
#                       else NA),
#            opt_warnings = map(model, get_warnings),
#            lme4_warnings = map(model, get_lme4_warnings))
# }

# tar_read(bootstraps) |> tail(1) |> fit_model() |> pull(lme4_warnings)

