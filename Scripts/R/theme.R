set_theme(theme_classic(base_size = 14))

j_gt <- function(data, ...) {
  data |>
    gt(...) |>
    tab_options(
      table.font.name = "Source Sans Pro",
      # heading.background.color = "#2C3E50",
      # column_labels.background.color = "#34495E",
      column_labels.font.weight = "bold",
      table.border.top.style = "none"
    )
}
