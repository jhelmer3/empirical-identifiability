make_plt_layout <- function(terms) {
  tibble(term = terms) |>
    mutate(plot_terms = imap(term, \(term, idx) tibble(xterm = term,
                                                       yterm = head(terms, idx))),
           n_terms = map_int(plot_terms, \(plot_terms) nrow(plot_terms)),
           n_prev_terms = map_int(n_terms, \(n_terms) seq(n_terms - 1, 0) |> sum()),
           # pat only worked as a way to setup the patchwork when length(terms) < 7
           pat = map2_chr(n_terms, n_prev_terms,
                          \(n_terms, n_prev_terms) paste0(LETTERS[seq(n_prev_terms + 1, length.out = n_terms)] |> 
                                                            paste(collapse = ""),
                                                          rep("#", times = length(terms) - n_terms) |> paste(collapse = ""))))
}