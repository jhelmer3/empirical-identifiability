
get_condition_plt_dim <- function(results_grouped) {
  model_id <- unique(results_grouped$model_id)
  
  recode_values(model_id,
                1 ~ 5.5,
                2 ~ 5.5,
                3 ~ 6,
                4 ~ 6.5,
                5 ~ 8) 
}
