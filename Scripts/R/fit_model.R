
fit_model <- function(bootstraps) {
  
  safe_specific_model <- function(data, model) {
    safely(
      \(data) lme4::lmer(model, data),
      otherwise = NA_real_
    )(data)
  }
  
  bootstraps |>
    mutate(res = map2(bootstrap, model_string, 
                      \(bootstrap, model_string) safe_specific_model(bootstrap, model_string)),
           model = map(res, "result"),
           error = map_chr(res, \(res) pluck(res, "error", "message", .default = NA_character_)),
           tidy = map2(model, error, \(model, error) 
                      if (is.na(error)) broom.mixed::tidy(model)
                      else NA),
           opt_warnings = map(model, get_warnings),
           lme4_warnings = map(model, get_lme4_warnings))
}

# tar_read(bootstraps) |> tail(1) |> fit_model() |> pull(lme4_warnings)
