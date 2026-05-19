ggplot2::set_theme(ggplot2::theme_classic(base_size = 14))

j_gt <- function(data, ...) {
  data |>
    gt(...) |>
    tab_options(
      # heading.background.color = "#2C3E50",
      # column_labels.background.color = "#34495E",
      column_labels.font.weight = "bold",
      table.border.top.style = "none"
    )
}
