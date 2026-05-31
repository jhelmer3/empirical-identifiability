format_plt_dat <- function(replication, xterm, yterm) {
  replication |>
    select(rep, opt_warnings, lme4_warnings, error, tidy) |>
    mutate(error_type = pmap_int(list(lme4_warnings,
                                      opt_warnings,
                                      error),
                                 \(lme4_warnings, opt_warnings, error)
                                 sum(!is.na(lme4_warnings),
                                     !is.na(opt_warnings),
                                     !is.na(error)))) |>
    unnest(tidy) |>
    filter(term %in% c(xterm, yterm)) |>
    select(rep, error_type, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    drop_na()
}

# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) 
# 
# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   format_plt_dat("(Intercept)", "private") |>
#   filter(if_any(everything(), is.na))
# 
# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   format_plt_dat("ses:private", "ses")
