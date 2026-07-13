make_plt_layout <- function(terms) {
  tibble(term = terms) |>
    mutate(plot_terms = imap(term, \(term, idx) tibble(yterm = term,
                                                       xterm = head(terms, idx))),
           n_terms = map_int(plot_terms, \(plot_terms) nrow(plot_terms)),
           n_prev_terms = map_int(n_terms, \(n_terms) seq(n_terms - 1, 0) |> sum()))
}