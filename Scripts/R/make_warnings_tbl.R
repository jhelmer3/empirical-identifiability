make_warnings_tbl <- function(results) {
  
  warning_patterns_rm <- c(
    "max\\|grad\\| = [\\d.]+",
    ": see help\\('isSingular'\\)?"
  ) |> str_c(collapse = "|")
  
  results |>
    # some of this should probably be moved to fit_model()
    mutate(
      lme4_warnings = map_chr(lme4_warnings, \(w)
                              str_replace_all(w, warning_patterns_rm, ""))
    ) |>
    mutate(across(c(opt_warnings, lme4_warnings, error),
                  \(col) map_chr(col, \(x) str_c(x, collapse = ", ")))) |>
    summarize(.by = c(opt_warnings, lme4_warnings, error),
              count = n(),
              prop =  n() / first(results |> pull(n_bootstraps))) |>
    arrange(desc(prop)) |>
    j_gt() |>
    tab_style(style = cell_text(weight = "bold"),
              location = cells_column_labels()) |>
    sub_missing(missing_text = "None") |>
    cols_label(prop = "%",
               count = "Count",
               opt_warnings = "Optimizer",
               lme4_warnings = md("`{lme4}`"),
               error = "Errors") |>
    cols_hide(count) |>
    tab_header("Warnings and Errors") |>
    fmt_percent(prop, decimals = 1) |>
    tab_style(cell_fill(color = "#e2eee2"),
              cells_body(rows = is.na(opt_warnings) & is.na(lme4_warnings) & is.na(error))) |>
    opt_horizontal_padding(scale = .2) |>
    tab_options(table.font.size = 13)
}

# tar_read(results_grouped) |>
#   filter(condition_id == 4) |>
#   make_warnings_tbl() |>
#   wrap_table()

# tar_read(results_grouped) |>
#   filter(condition_id == 1) |>
#   mutate(
#     lme4_warnings_trimmed = map_chr(lme4_warnings, \(w)
#                             str_replace_all(w, warning_patterns_rm, ""))
#   ) |>
#   select(starts_with("lme4"))
# 
# tar_read(results_grouped) |>
#   filter(condition_id == 1) |>
#   mutate(lme4_warnings = map_chr(lme4_warnings, \(w)
#                                          str_replace_all(w, warning_patterns_rm, ""))) |>
#   mutate(across(c(opt_warnings, lme4_warnings, error),
#                 \(col) map_chr(col, \(x) str_c(x, collapse = ", ")))) |>
#   summarize(.by = c(opt_warnings, lme4_warnings, error),
#             count = n()) |>
#   str()

# tar_read(results_grouped) |>
#   # mimic grouping
#   filter(condition_id ==  1) |>
#   mutate(
#     lme4_warnings = map(lme4_warnings, 
#                         \(lme4_warnings) str_replace_all(lme4_warnings, 
#                                                          regex(warning_patterns_rm),
#                                                          ""))
#   ) |>
#   summarize(.by = c(opt_warnings, lme4_warnings, error),
#             count = n(),
#             prop =  n() / first(tar_read(results_grouped) |>
#                                   # mimic grouping
#                                   filter(condition_id ==  1) |> pull(n_bootstraps))) |>
#   arrange(desc(prop)) 
  

# d <- tibble(n_bootstraps = 100,
#             n_students = 2,
#             n_schools = 3) |>
#   get_bootres()
# 
# make_warnings_tbl(d)

# d |>
#   summarize(.by = c(opt_warnings, lme4_warnings, error),
#             count = n(),
#             prop =  n() / first(param_n_bootstraps)) |>
#   arrange(desc(prop))
# 
# d |>
#   select(lme4_warnings) |>
#   count(lme4_warnings)
# 

  