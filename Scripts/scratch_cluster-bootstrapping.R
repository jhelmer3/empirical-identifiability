
library(tidyverse)

n_clusters <- 100
n_per_cluster <- 50

population_intercept <- 0
cluster_effect_sd <- 1
indiv_effect_id <- 1

params_key <- tribble(~term, ~param, ~true_value,
                      "(Intercept)", "population_intercept", population_intercept,
                      "sd__(Intercept)", "cluster_effect_sd", cluster_effect_sd,
                      "sd__Observation", "indiv_effect_id", indiv_effect_id)

d <- tibble(cluster_id = 1:n_clusters,
       n_per_cluster = rep(n_per_cluster, n_clusters),
       cluster_effect = rnorm(n_clusters, 0, cluster_effect_sd)) |>
  uncount(n_per_cluster, .remove = F) |>
  mutate(indiv_id = row_number(),
         indiv_effect = rnorm(n_per_cluster * n_clusters, 0, indiv_effect_id),
         y = population_intercept + cluster_effect + indiv_effect) 

true_values <- d |>
  lme4::lmer(y ~ 1 + (1 | cluster_id), data = _) |>
  broom.mixed::tidy() |>
  select(-c(effect, group, statistic)) |>
  pivot_longer(c(estimate, std.error),
               names_to = "type", values_to = "true_estimate") |>
  filter_out(is.na(true_estimate))
  

n_iter <- 50

res <- bind_rows(
  map(1:n_iter, 
      \(i)
      ## getting bootstrapped estimates
      map(1:100, 
          \(bootstrap_rep) 
          d |>
            filter(cluster_id %in% sample(cluster_id, 100, replace = T)) |>
            filter(.by = cluster_id, 
                   indiv_id %in% sample(indiv_id, 50, replace = T)) |>
            lme4::lmer(y ~ 1 + (1 | cluster_id), data = _) |>
            broom.mixed::tidy()) |>
        list_rbind() |>
        summarize(.by = term,
                  std.error = sd(estimate),
                  estimate = mean(estimate)) |>
        pivot_longer(c(estimate, std.error),
                     names_to = "type", values_to = "estimate") |>
        inner_join(true_values, by = c("term", "type")) |>
        mutate(rep = i,
               bias = estimate - true_estimate,
               method = "two_stage")
  ) |>
    list_rbind(),
  map(1:n_iter, 
      \(i)
      ## getting bootstrapped estimates
      map(1:100, 
          \(bootstrap_rep) 
          d |>
            filter(cluster_id %in% sample(cluster_id, 100, replace = T)) |>
            lme4::lmer(y ~ 1 + (1 | cluster_id), data = _) |>
            broom.mixed::tidy()) |>
        list_rbind() |>
        summarize(.by = term,
                  std.error = sd(estimate),
                  estimate = mean(estimate)) |>
        pivot_longer(c(estimate, std.error),
                     names_to = "type", values_to = "estimate") |>
        inner_join(true_values, by = c("term", "type")) |>
        mutate(rep = i,
               bias = estimate - true_estimate,
               method = "cluster")
  ) |>
    list_rbind()
)

res |>
  ggplot(aes(x = bias, color = method)) +
  geom_density() +
  facet_grid(term ~ type, scales = "free_y") +
  theme_classic()




