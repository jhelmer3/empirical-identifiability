
fmt_spaghetti_dat <- function(lme4_model) {
  populaton_effects <- lme4_model |>
    broom.mixed::tidy() |>
    filter(when_any(term == "(Intercept)",
                    term == "ses")) |>
    select(term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate)
  
  school_effects <- lme4_model |>
    lme4::ranef() |>
    as_tibble() |>
    mutate(term = paste0(term, "_school_effect")) |>
    select(grp, term, estimate = condval) |>
    pivot_wider(names_from = term, values_from = estimate)
  
  bind_cols(populaton_effects,
            school_effects) |>
    mutate(school_intercept = `(Intercept)` + `(Intercept)_school_effect`,
           school_ses_slope = ses + ses_school_effect) |>
    select(grp, school_intercept, ses_school_effect) |>
    crossing(ses = -3:3) |>
    mutate(mathscore_pred = school_intercept + ses_school_effect * ses)
}

plt_spaghetti <- function(fmtted_spaghetti_dat) {
  fmtted_spaghetti_dat |>
    ggplot(aes(x = ses, y = mathscore_pred, group = grp)) +
    geom_line(alpha = 0.1) +
    scale_x_continuous(limits = c(-2.5, 2.5), breaks = c(-2.5, -1, 0, 1, 2.5)) +
    guides(x = guide_axis(cap = T),
           y = guide_axis(cap = T)) +
    labs(x = "School SES", y = "Predicted Math Score") +
    theme(aspect.ratio = 1)
}

make_spaghetti_plt <- function(lme4_model) {
  fmtted_spaghetti_dat <- fmt_spaghetti_dat(lme4_model)
  plt_spaghetti(fmtted_spaghetti_dat)
}

# make_spaghetti_plt(tar_read(full_conditional_model))
#   
# tar_read(results) |> slice(10) |> pluck("model", 1) |>
#   make_spaghetti_plt()
