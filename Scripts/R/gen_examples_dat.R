
gen_examples_dat <- function(data_gen_params) {
  data_gen_vcov <- matrix(c(data_gen_params$`sd__(Intercept)`,
                                             0,
                                             0,
                                             data_gen_params$`sd__ses`),
                                           ncol = 2, byrow = T) %*%
    matrix(c(1,
             data_gen_params$`cor__(Intercept).ses`,
             data_gen_params$`cor__(Intercept).ses`,
             1),
           ncol = 2, byrow = T) %*%
    matrix(c(data_gen_params$`sd__(Intercept)`,
             0,
             0,
             data_gen_params$`sd__ses`),
           ncol = 2, byrow = T)
  
    tibble(example_type = data_gen_params$example_type,
           n_bootstraps = data_gen_params$n_bootstraps,
           iter = data_gen_params$iter,
           schoolid = seq(1, data_gen_params$n_schools),
           n_students = data_gen_params$n_students,
           n_schools = data_gen_params$n_schools,
           private = rbinom(data_gen_params$n_schools, 1, .3),
           beta_ses = data_gen_params$ses,
           beta_private = data_gen_params$private,
           beta_private_by_ses = data_gen_params$`ses:private`) |>
      bind_cols(varying_effects = MASS::mvrnorm(data_gen_params$n_schools,
                                                mu = c(data_gen_params$`(Intercept)`, data_gen_params$ses),
                                                Sigma = data_gen_vcov) |>
                  as_tibble() |>
                  set_names(c("alpha_school", "beta_ses_school"))) |>
      uncount(n_students, .remove = F) |>
      mutate(studentid = row_number(),
             .after = schoolid) |>
      mutate(ses = rnorm(data_gen_params$n_schools * data_gen_params$n_students),
             private_by_ses = private * ses,
             mu_school = alpha_school +
               beta_private * private +
               beta_private_by_ses * private_by_ses,
             mu = mu_school + beta_ses * ses,
             mathscore = rnorm(data_gen_params$n_schools * data_gen_params$n_students,
                               mean = mu, sd = data_gen_params$sd__Observation)) |>
      nest(gen_params = c(starts_with("beta"), starts_with("alpha"), starts_with("mu")),
           bootstrap = !c(example_type, n_bootstraps, n_students, n_schools)) 
}

# tar_read(examples_design) |>
#   head(1) |>
#   gen_examples_dat() 

# tar_read(examples_design) |>
#   head(1) -> data_gen_params
# 
# tibble(example_type = data_gen_params$example_type,
#        iter = data_gen_params$iter,
#        schoolid = seq(1, data_gen_params$n_schools),
#        n_students = data_gen_params$n_students,
#        private = rbinom(data_gen_params$n_schools, 1, .3),
#        beta_ses = data_gen_params$ses,
#        beta_private = data_gen_params$private,
#        beta_private_by_ses = data_gen_params$`ses:private`)
# 
#   ggplot(aes(x = ses, y = mathscore, color = factor(private))) +
#   geom_point()
