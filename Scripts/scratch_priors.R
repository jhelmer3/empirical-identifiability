
plt_rmse <- function(results) {
  tar_read(results) |>
    select(model) |>
    mutate(
      rmse = map_dbl(model, \(model) {
        performance::performance_rmse(model)
      })
    )
}

tar_read(results_grouped) |>
  plt_rmse()
tar_read(results_grouped_v_subset) |> 
  pluck("model", 1) |> 
  performance::performance_rmse()

d <- tar_read(results_grouped) |>
  select(model) |>
  mutate(
    rmse = map_dbl(model, \(model) {
      performance::performance_rmse(model)
    })
  )


d |> ggplot(aes(x = rmse)) +
  geom_density()

brms::get_prior(mathscore ~ 1 + (1 | schoolid), data = tar_read(ecls_dat))

y <- tar_read(ecls_dat)$mathscore

b1 <- brms::brm(mathscore ~ 1 + (1 | schoolid),
          data = tar_read(ecls_dat), sample_prior = "only")
yrep_b1 <- brms::posterior_predict(b1)
pp_b1 <- bayesplot::pp_check(y, yrep = yrep_b1[50:150, ], bayesplot::ppc_dens_overlay)

brms::get_prior(b1)

b2 <- brms::brm(mathscore ~ 1 + (1 | schoolid),
                data = tar_read(ecls_dat), 
                prior = c(brms::prior(student_t(3, -0.3, 5), class = Intercept),
                          brms::prior(student_t(3, 0, 5), class = sd),
                          brms::prior(student_t(3, 0, 5), class = sigma)),
                sample_prior = "only")
yrep_b2 <- brms::posterior_predict(b2)
pp_b2 <- bayesplot::pp_check(y, yrep = yrep_b2[50:150, ], bayesplot::ppc_dens_overlay)

pp_b1 / pp_b2


tibble(x = seq(-50, 50),
       p = map(x, \(x) tribble(~dist, ~p,
                   "t_3_0_5", dt(x, 3) * 5 + 0.3,
                   "t_3_0_2.5", dt(x, 3) * 2.5 + 0.3,
                   "t_3_0_1", dt(x, 3) * 1 + 0.3,
                   "unif_25", dunif(x, -10, 10)))) |>
  unnest(p) |>
  ggplot(aes(x = x, y = p, color = dist)) +
  geom_line()

