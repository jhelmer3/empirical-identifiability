
bootstrap_subset <- function(params_w_subset) {
  params_w_subset |>
    uncount(n_bootstraps, .remove = F) |>
    mutate(
      bootstrap = pmap(
        list(n_students, n_schools, data_subset),
        \(n_students, n_schools, data_subset) 
        data_subset |>
          filter(schoolid %in% sample(schoolid, params_w_subset$n_schools, 
                                      replace = T)) |>
          filter(.by = schoolid,
                 childid %in% sample(childid, params_w_subset$n_students,
                                     replace = T))
      )
    )
}







# n_bootstraps <- 100
# 
# d <- tibble(cluster_id = 1:5,
#        n_indivs = c(3, 5, 7, 10, 15)) |>
#   uncount(n_indivs, .remove = F, .id = "n_within") |>
#   mutate(indiv_id = paste0(cluster_id, n_within))
# 
# bs <- d |>
#   modelr::bootstrap(n_bootstraps) |>
#   mutate(strap = map(strap, as_tibble)) |>
#   unnest(strap)
# 
# bs <- d |> 
#   nest() |>
#   mutate(n_bootstraps = n_bootstraps) |>
#   uncount(n_bootstraps) |>
#   mutate(strap = map(data,
#                      \(data) data |>
#                        filter(indiv_id %in% sample(indiv_id, 40)))) |>
#   unnest(strap)
# 
# bs |>
#   summarize(.by = indiv_id,
#             n = n(),
#             p = n / n_bootstraps,
#             n_indivs = first(n_indivs)) |>
#   ggplot(aes(x = indiv_id, y = p, fill = n_indivs)) +
#   geom_col() +
#   guides(x = guide_axis(cap = T),
#          y = guide_axis(cap = T)) +
#   coord_cartesian(ylim = c(0, .5)) +
#   labs(subtitle = "`modelr::bootstrap()` gives every observation an equal probability of being sampled." |>
#          str_wrap(),
#        y = "Proportion of Times Sampled",
#        x = "Individual ID",
#        fill = "Cluster Size",
#        caption = glue::glue("Based on {n_bootstraps} bootstraps")) +
#   scale_fill_distiller(palette = "RdPu", direction = 1,
#                        breaks = unique(d$n_indivs)) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   scale_x_discrete(labels = NULL) +
#   theme(legend.position = "bottom")
