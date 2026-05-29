
get_examples_dat <- function(results, model_terms) {
  potential_examples <- results |>
    select(rep, condition_id, tidy) |>
    mutate(tidy = map(tidy, \(tidy) tidy |> 
                        complete(term = model_terms))) |>
    unnest(tidy) |>
    filter_out(.by = rep, any(is.na(estimate)))
  
  ex_bad_id <- potential_examples |>
    filter(when_all(condition_id == 1, term == "sd__ses")) |>
    filter(estimate == max(estimate, na.rm = T)) |>
    pull(rep)
  
  ex_good_id <- potential_examples |>
    filter(when_all(condition_id == 4, term == "sd__ses")) |>
    mutate(dist_from_mean = abs(estimate - mean(estimate, na.rm = T))) |>
    filter(dist_from_mean == min(dist_from_mean)) |>
    pull(rep)
  
  tibble(rep = c(ex_bad_id, ex_good_id),
         condition_id = c(1, 4),
         example_type = c("bad", "good")) |>
    left_join(results,
              by = c("rep", "condition_id"))
}

# tar_read(results_grouped) |>
#   get_examples_dat(tar_read(model_terms)) 
 
# tar_read(results_grouped) |>
#   filter(condition_id == 1) |>
#   select(rep, condition_id, tidy) |>
#   unnest(tidy) |>
#   filter(when_all(condition_id == 1)) |> View()
# issue: Need to find a way to make sure selected examples have estimates for all parameters

# 
# tar_read(results_grouped) |>
#   get_examples_dat(tar_read(model_terms)) |>
#   select(example_type, tidy) |>
#   unnest(tidy)
