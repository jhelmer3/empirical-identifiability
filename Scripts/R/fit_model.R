
fit_model <- function(bootstraps) {
  bootstraps |>
    mutate(res = map(bootstrap, safe_model),
           model = map(res, "result"),
           error = map_chr(res, \(res) pluck(res, "error", "message", .default = NA_character_)),
           tidy = map2(model, error, \(model, error) 
                      if (is.na(error)) broom.mixed::tidy(model)
                      else NA),
           opt_warnings = map(model, get_warnings),
           lme4_warnings = map(model, get_lme4_warnings))
}

# tar_read(bootstraps) |> tail(1) |> fit_model() |> pull(lme4_warnings)
