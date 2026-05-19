
define_params <- function(params) {
  do.call(crossing, params) |>
    mutate(condition_id = row_number(),
           .before = everything())
}

#tar_read(param_set)
