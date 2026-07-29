
init_params <- function(n_bootstraps, models) {
  list(
    n_bootstraps =  n_bootstraps,
    n_students = c(3, 10),
    n_schools = c(10, 30),
    model_string = models,
    centered = c(T, F)
  )
}