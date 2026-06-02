
reps <- 10000

n_clusters <- 4

d <- tibble(cluster = seq(1, n_clusters),
       n_within = c(3, 4, 5, 12)) |>
  uncount(n_within, .remove = F) |>
  mutate(indiv = row_number())

bs <- map(seq(1, reps), \(i)
    d |>
      filter(cluster %in% sample(n_clusters, 2)) |>
      filter(.by = cluster,
             indiv %in% sample(indiv, 2)) |>
      mutate(rep = i)) |>
  list_rbind()

props <- bs |> 
  summarize(.by = indiv,
            n = n(),
            p = n / reps,
            n_within = first(n_within))
props |>
  ggplot(aes(x = indiv, y = p, fill = n_within)) +
  geom_col() +
  guides(x = guide_axis(cap = T),
         y = guide_axis(cap = T)) +
  coord_cartesian(ylim = c(0, .5)) +
  labs(subtitle = "Individuals in clusters of greater size have less chance of being selected in an equal-probability two-stage bootstrap procedure." |>
         str_wrap(),
       y = "Proportion of Times Sampled",
       x = "Individual ID",
       fill = "Cluster Size",
       caption = glue::glue("Based on {reps} replications.")) +
  scale_fill_distiller(palette = "RdPu", direction = 1,
                       breaks = unique(props$n_within)) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(legend.position = "bottom")
