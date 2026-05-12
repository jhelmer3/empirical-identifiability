make_warnings_tbl <- function(bootres) {
  
  warning_patterns <- c(
    "Model failed to converge",
    "boundary \\(singular\\) fit"
  )
  
  bootres |>
    mutate(
      lme4_warnings = str_extract(lme4_warnings, str_c(warning_patterns, collapse = "|")) |>
        coalesce(lme4_warnings)) |>
    summarize(.by = c(opt_warnings, lme4_warnings),
              count = n(),
              prop =  n() / first(param_n_bootstraps)) |>
    arrange(desc(prop)) |>
    j_gt() |>
    tab_style(style = cell_text(weight = "bold"),
              location = cells_column_labels()) |>
    sub_missing(missing_text = "None") |>
    cols_label(prop = "%",
               count = "Count",
               opt_warnings = "Optimizer",
               lme4_warnings = md("`{lme4}`")) |>
    cols_hide(count) |>
    tab_header("Warnings") |>
    fmt_percent(prop, decimals = 1) |>
    tab_style(cell_fill(color = "#e2eee2"),
              cells_body(rows = is.na(opt_warnings) & is.na(lme4_warnings))) |>
    tab_options(table.font.size = 14) 
}

# d <- tibble(n_bootstraps = 100,
#             n_students = 2,
#             n_schools = 3) |>
#   get_bootres()
# 
# d |>
#   select(lme4_warnings) |>
#   count(lme4_warnings)
# 
# make_warnings_tbl(d)
