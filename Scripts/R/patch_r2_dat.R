
patch_r2_dat <- function(r2_dat) {
  r2_dat |>
    map(\(data) {
      error_dat <- data |>
        mutate(r2_error = is.na(r2_estimate)) |>
        summarize(.by = r2_type,
                  percent_removed = mean(r2_error) * 100) |>
        mutate(string = glue::glue("{round(percent_removed, 2)}% {r2_type} R^2s errored"))
      
      data |>
        ggplot(aes(x = r2_estimate, color = r2_type)) +
        geom_density(na.rm = T) +
        geom_text(data = error_dat,
                  aes(label = string,
                      x = -Inf, y = Inf, vjust = c(1, 2.3), hjust = 0)) +
        scale_x_continuous("R^2", limits = c(0, 1), breaks = seq(0, 1, .2)) +
        scale_y_continuous(NULL, breaks = NULL) +
        guides(x = guide_axis(cap = T),
               color = guide_none()) +
        theme(aspect.ratio = (1.618)^-1,
              axis.line.y = element_blank(),
              axis.title.x = ggtext::element_markdown())
    }) |>
    wrap_plots(ncol = 2, axes = "collect_x")
}
# 
# tar_read(r2_dat) |>
#   patch_r2_dat() |> class()
