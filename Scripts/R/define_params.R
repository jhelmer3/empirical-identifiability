
define_params <- function(params, models) {
  do.call(crossing, params) |>
    # get rid of combinations that are the full data n_students or n_schools
    # crossed with an experimental condition n_students or n_schools
    # filter_out(
    #   when_any(
    #     when_all(
    #       n_students == max(n_students),
    #       n_schools != max(n_schools)
    #     ),
    #     when_all(
    #       n_students != max(n_students),
    #       n_schools == max(n_schools)
    #     )
    #   )
    # ) |>
    mutate(
      model_string = factor(model_string, levels = models, ordered = T)
    ) |>
    arrange(model_string) |>
    mutate(.by = model_string,
           model_id = cur_group_id()) |>
    mutate(condition_id = row_number(),
           .before = everything()) |>
    mutate(.by = c(model_id, centered, bootstrap_type, full_data), 
           btwn_condition_id = cur_group_id())
}

# tar_read(parkeep_full_data = ifelse(n_students == max(n_students) & n_schools == max(n_schools), T, F)ams)

# tar_read(param_set) |>
#   define_params(tar_read(models)) |>
#   pull(model_string) |>
#   levels()

# tar_read(param_set) |>
#   define_params(tar_read(models)) |>
#   select(model_string, model_id) 

# do.call(crossing, tar_read(param_set)) 
  
