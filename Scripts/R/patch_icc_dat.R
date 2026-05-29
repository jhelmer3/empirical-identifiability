
patch_icc_dat <- function(icc_dat) {
  
  icc_dat |>
    map(\(data) data |>
          ggplot(aes(x = icc)) +
          geom_density(na.rm = T) +
          geom_text(data = data |>
                      mutate(icc_error = is.na(icc)) |>
                      summarize(percent_removed = mean(icc_error) * 100),
                    aes(label = glue::glue("{round(percent_removed, 2)}% ICCs errored"),
                        x = -Inf, y = Inf, vjust = 1, hjust = 0)) +
          scale_x_continuous("ICC", limits = c(0, 1), breaks = seq(0, 1, .2)) +
          scale_y_continuous(NULL, breaks = NULL) +
          guides(x = guide_axis(cap = T)) +
          theme(aspect.ratio = (1.618)^-1,
                axis.line.y = element_blank())) |>
    wrap_plots(ncol = 2, axes = "collect_x")
}