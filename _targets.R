
library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_option_set(
  packages = c("tidyverse", "patchwork", "gt"),
  controller = crew_controller_local(workers = 4),
  format = "qs",
  seed = 123456
)

tar_source(here::here("Scripts", "R"))

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

n_bootstraps <- 240

list(
  tar_target(ecls_file,
             here::here("..", "Data", "ECLS-K", "ECLS-K.rds"),
             format = "file"),
  tar_target(ecls, readRDS(ecls_file)),
  tar_target(ecls_dat, clean_ecls(ecls)),
  tar_target(models, c("mathscore ~ 1 + (1 | schoolid)",
                       "mathscore ~ ses + (1 | schoolid)",
                       "mathscore ~ ses + rural + (1 | schoolid)",
                       "mathscore ~ ses * rural + (1 | schoolid)",
                       "mathscore ~ ses * rural + (ses | schoolid)")),
  tar_target(full_data_models, 
             tibble::tibble(model_string = models,
                            full_res = purrr::map(models, \(model) lme4::lmer(model, data = ecls_dat)),
                            full_tidy = purrr::map(full_res, \(res) broom.mixed::tidy(res)),
                            terms = purrr::map(full_tidy, "term"),
                            plt_layout = purrr::map(terms, make_plt_layout)),
             pattern = map(models)),
  
  tar_target(param_set, init_params(n_bootstraps, models)),
  tar_target(params, define_params(param_set, models) |>
               dplyr::left_join(full_data_models)),
  
  ## subsetting once per condition, bootstrapping within
  ## still need to overall replicate this multiple times
  tar_target(params_w_subset, subset_dat(ecls_dat, params),
             pattern = map(params)),
  tar_target(results, bootstrap_subset(params_w_subset) |>
               fit_model(),
             pattern = map(params_w_subset)),
  
  tar_target(axis_limits, identify_axis_limits(results)),
  tar_group_by(results_grouped, results |> 
                 dplyr::mutate(.by = condition_id,
                               rep = row_number()), 
               condition_id),

  tar_target(condition_plt, 
             patch_plt_dat(results_grouped, axis_limits),
             pattern = map(results_grouped),
             iteration = "list"),
  tar_target(condition_plt_files,
             paste0("outputs/condition_", first(results_grouped$condition_id),
                    "_model_", first(results_grouped$model_id), ".png") |>
               ggsave_and_return_path(condition_plt, 
                                      width = get_condition_plt_dim(results_grouped), 
                                      height = get_condition_plt_dim(results_grouped)),
             pattern = map(condition_plt, results_grouped),
             iteration = "list",
             format = "file"),
  
  tar_target(icc_dat, 
             make_icc_dat(results_grouped),
             pattern = map(results_grouped)),
  tar_group_by(icc_dat_grouped_by_model, icc_dat, model_id),
  tar_target(icc_plts,
             patch_icc_dat(icc_dat_grouped_by_model),
             pattern = map(icc_dat_grouped_by_model),
             iteration = "list"),
  
  tar_target(r2_dat, 
             make_r2_dat(results_grouped),
             pattern = map(results_grouped)),
  tar_group_by(r2_dat_grouped_by_model, r2_dat, model_id),
  tar_target(r2_plts,
             patch_r2_dat(r2_dat_grouped_by_model),
             pattern = map(r2_dat_grouped_by_model),
             iteration = "list"),
  # tar_target(examples_dat,
  #            get_examples_dat(results_grouped, model_terms)),
  # tar_target(examples_params, 
  #            get_examples_params(examples_dat, n_bootstraps),
  #            pattern = map(examples_dat)),
  # tar_target(examples_design,
  #            vctrs::vec_c(examples_params)),
  # tar_target(examples_results,
  #            gen_examples_dat(examples_design) |>
  #              fit_model(),
  #            pattern = map(examples_design)),
  # tar_target(examples_axis_limits,
  #            identify_axis_limits(examples_results)),
  # tar_group_by(examples_results_grouped, 
  #              examples_results |>
  #                dplyr::mutate(.by = example_type,
  #                              rep = row_number()),
  #              example_type),
  # tar_target(examples_spaghetti_plts, 
  #            examples_results_grouped |>
  #              pluck("model", 1) |> 
  #              make_spaghetti_plt(),
  #            pattern = map(examples_results_grouped),
  #            iteration = "list"),
  # tar_target(examples_condition_plt,
  #            patch_plt_dat(examples_results_grouped, plt_layout, true_values, examples_axis_limits,
  #                          title_info_vars = c("example_type", "n_bootstraps", 
  #                                              "n_students", "n_schools")),
  #            pattern = map(examples_results_grouped),
  #            iteration = "list"),
  # tar_target(examples_condition_plt_files,
  #            paste0("outputs/examples_condition_plt_", 
  #                   targets::tar_name(), ".png") |>
  #              ggsave_and_return_path(examples_condition_plt, 
  #                                     width = 8, height = 8),
  #            pattern = map(examples_condition_plt),
  #            iteration = "list",
  #            format = "file"),
  # tar_target(examples_icc_dat, 
  #            make_icc_dat(examples_results_grouped),
  #            pattern = map(examples_results_grouped),
  #            iteration = "list"),
  # tar_target(examples_icc_plt,
  #            patch_icc_dat(examples_icc_dat)),
  # tar_target(examples_r2_dat, 
  #            make_r2_dat(examples_results_grouped),
  #            pattern = map(examples_results_grouped),
  #            iteration = "list"),
  # tar_target(examples_r2_plt,
  #            patch_r2_dat(examples_r2_dat)),
  tar_quarto(ecls_report, "ecls-k.qmd",
             quiet = F)
)
