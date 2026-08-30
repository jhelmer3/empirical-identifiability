
bootstrap_subset <- function(params_w_subset) {
  bootstrap_type <- params_w_subset["bootstrap_type"][[1]][1]
  
  all_bootstraps <- params_w_subset
  
  if (bootstrap_type == "both") {
    all_bootstraps |>
      mutate(
        bootstrap = pmap(
          list(n_students, n_schools, data_subset),
          \(n_students, n_schools, data_subset) 
          data_subset |>
            # nest all data within schools, to
            nest(cluster_data = -schoolid) |>
            # sample n_schools schools, then
            slice_sample(n = n_schools, replace = T) |>
            # sample n_students students within schools, and finally
            mutate(cluster_data = map(
              cluster_data, \(cluster_data) 
              cluster_data |>
                slice_sample(n = n_students, replace = T))
            ) |>
            # unnest the data within schools
            unnest(cluster_data)
        )
      )
  } else if (bootstrap_type == "cluster") {
    all_bootstraps |>
      mutate(
        bootstrap = pmap(
          list(n_schools, data_subset),
          \(n_schools, data_subset) 
          data_subset |>
            # nest all data within schools, to
            nest(cluster_data = -schoolid) |>
            # sample n_schools schools, then
            slice_sample(n = n_schools, replace = T) |>
            # unnest the data within schools
            unnest(cluster_data)
        )
      )
  } else if (bootstrap_type == "individual") {
    all_bootstraps |>
      mutate(
        bootstrap = pmap(
          list(n_students, n_schools, data_subset),
          \(n_students, n_schools, data_subset) 
          data_subset |>
            # sample dataset size (n_students * n_schools) students
            slice_sample(n = n_students * n_schools, replace = T)
        )
      )
  }
}


# 
# tar_read(params_w_subset) |>
#   head(1) |>
#   bootstrap_subset() |>
#   pull(bootstrap)



# n_bootstraps <- 100
# 
# d <- tibble(cluster_id = 1:5,
#        n_indivs = c(3, 3, 3, 3, 3)) |>
#   uncount(n_indivs) |>
#   mutate(indiv_id = row_number())
# d
# 
# set.seed(1)
# d |>
#   nest(cluster_data = -cluster_id) |>
#   slice_sample(n = 3, replace = T) |>
#   mutate(cluster_data = map(cluster_data, \(cluster_data) cluster_data |>
#                                       slice_sample(n = 3, replace = T))) |>
#   unnest(cluster_data)

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
