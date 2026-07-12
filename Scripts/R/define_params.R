
define_params <- function(params, models) {
  do.call(crossing, params) |>
    mutate(model_string = factor(model_string, levels = models, ordered = T)) |>
    arrange(model_string) |>
    mutate(.by = model_string,
           model_id = cur_group_id()) |>
    mutate(condition_id = row_number(),
           .before = everything())
}

# tar_read(param_set) |>
#   define_params(tar_read(models)) |>
#   pull(model_string) |>
#   levels()

# tar_read(param_set) |>
#   define_params(tar_read(models)) |>
#   select(model_string, model_id) 
