
make_plt_dat <- function(bootres) {
  axis_label_width <- 5
  
  bootres |> pull(tidy) |> pluck(1) |> pull(term) |> unique() |>
    bs_plt_layout() |>
    mutate(plots = map(plot_terms,
                       \(plot_terms) plot_terms |>
                         mutate(plot = map2(xterm, yterm,
                                            \(xterm, yterm) {
                                              df <- bootres |>
                                                select(opt_warnings, lme4_warnings, error, tidy) |>
                                                mutate(error_type = pmap_int(list(lme4_warnings,
                                                                                  opt_warnings,
                                                                                  error),
                                                                             \(lme4_warnings, opt_warnings, error)
                                                                             sum(!is.na(lme4_warnings),
                                                                                 !is.na(opt_warnings),
                                                                                 !is.na(error)))) |>
                                                unnest(tidy) |>
                                                filter(term %in% c(xterm, yterm)) |>
                                                select(error_type, term, estimate, iter) |>
                                                pivot_wider(names_from = term, values_from = estimate)
                                              
                                              mapping <- if (xterm == yterm) {
                                                do.call(aes, list(x = as.name(xterm)))
                                              } else {
                                                do.call(aes, list(y = as.name(xterm), x = as.name(yterm)))
                                              }
                                              geom <- if (xterm == yterm) geom_density() else geom_point(shape = 16,
                                                                                                         alpha = 0.3)
                                              ggplot(df, mapping) + 
                                                {if (xterm == yterm) geom_density() else geom_point(data = df,
                                                                                                    aes(color = factor(error_type),
                                                                                                        shape = factor(error_type)),
                                                                                                    shape = 16,
                                                                                                    alpha = 0.3)} +
                                                scale_x_continuous(breaks = NULL) +
                                                scale_y_continuous(breaks = NULL) +
                                                scale_color_manual(values = c("0" = "black",
                                                                              "1" = "orange",
                                                                              "2" = "red",
                                                                              "3" = "purple")) +
                                                labs(
                                                  x = if (xterm == yterm) wrap_axis_labels(xterm) else 
                                                    wrap_axis_labels(yterm),
                                                  y = if (xterm == yterm) NULL else 
                                                    wrap_axis_labels(xterm)
                                                ) +
                                                theme_classic() +
                                                theme(legend.position = "none")
                                            }))  |> pluck("plot")))
}
# 
# bs_dat |>
#   pluck(1) |>
#   make_plt_dat() |>
#   pull(plots) 

# make_plt_dat <- function(bootres) {
#   axis_label_width <- 5
#   
#   bootres |> pull(tidy) |> pluck(1) |> pull(term) |> unique() |>
#     bs_plt_layout() |>
#     mutate(plots = map(plot_terms,
#                        \(plot_terms) plot_terms |>
#                          mutate(plot = map2(xterm, yterm,
#                                             \(xterm, yterm) {
#                                               df <- bootres |>
#                                                 select(opt_warnings, lme4_warnings, error, tidy) |>
#                                                 mutate(error_type = !is.na(opt_warnings) + !is.na(lme4_warnings) + !is.na(error)) |>
#                                                 unnest(tidy) |>
#                                                 filter(term %in% c(xterm, yterm)) |>
#                                                 select(error_type, term, estimate, iter) |>
#                                                 pivot_wider(names_from = term, values_from = estimate)
#                                               
#                                               mapping <- if (xterm == yterm) {
#                                                 do.call(aes, list(x = as.name(xterm)))
#                                               } else {
#                                                 do.call(aes, list(y = as.name(xterm), x = as.name(yterm)))
#                                               }
#                                               geom <- if (xterm == yterm) geom_density() else geom_point(shape = 16,
#                                                                                                          alpha = 0.3)
#                                               
#                                               ggplot(df, mapping) + 
#                                                 geom +
#                                                 {if (xterm == yterm) NULL else geom_point(data = df |>
#                                                                                             filter_out(error_type == 0),
#                                                                                           aes(color = factor(error_type),
#                                                                                               shape = factor(error_type)))} +
#                                                 scale_x_continuous(breaks = NULL) +
#                                                 scale_y_continuous(breaks = NULL) +
#                                                 scale_color_manual(values = c("2" = "orange",
#                                                                               "3" = "red")) +
#                                                 labs(
#                                                   x = if (xterm == yterm) wrap_axis_labels(xterm) else 
#                                                     wrap_axis_labels(yterm),
#                                                   y = if (xterm == yterm) NULL else 
#                                                     wrap_axis_labels(xterm)
#                                                 ) +
#                                                 theme_classic()
#                                             }))  |> pluck("plot")))
# }

