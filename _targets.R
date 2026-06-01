
library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_option_set(
  packages = c("tidyverse", "patchwork", "gt"),
  controller = crew_controller_local(workers = 6),
  format = "qs"
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
  tar_target(full_unconditional_model,
             lme4::lmer(mathscore ~ 1 + (1 | schoolid), data = ecls_dat)),
  tar_target(full_conditional_model,
             safe_model(ecls_dat)$result),
  tar_target(true_values,
             broom.mixed::tidy(full_conditional_model)),
  tar_target(model_terms, full_conditional_model |> broom.mixed::tidy() |> pull(term)),
  tar_target(full_conditional_spaghetti_plt, make_spaghetti_plt(full_conditional_model)),
  tar_target(plt_layout, make_plt_layout(model_terms)),
  
  tar_target(param_set, init_params(n_bootstraps)),
  tar_target(params, define_params(param_set)),
  tar_rep(results, ecls_dat |> 
            bootstrap_data(params) |>
            fit_model(), 
          reps = n_bootstraps / 6, 
          batches = 6),
  tar_target(axis_limits, identify_axis_limits(results)),
  tar_group_by(results_grouped, results |> 
                 dplyr::mutate(.by = condition_id,
                               rep = row_number()), 
               condition_id),
  tar_target(condition_plt, 
             patch_plt_dat(results_grouped, plt_layout, true_values, axis_limits),
             pattern = map(results_grouped),
             iteration = "list"),
  tar_target(icc_dat, 
             make_icc_dat(results_grouped),
             pattern = map(results_grouped),
             iteration = "list"),
  tar_target(icc_plt,
             patch_icc_dat(icc_dat)),
  tar_target(r2_dat, 
             make_r2_dat(results_grouped),
             pattern = map(results_grouped),
             iteration = "list"),
  tar_target(r2_plt,
             patch_r2_dat(r2_dat)),
  tar_target(examples_dat,
             get_examples_dat(results_grouped, model_terms)),
  tar_target(examples_params, 
             get_examples_params(examples_dat, n_bootstraps),
             pattern = map(examples_dat)),
  tar_target(examples_design,
             vctrs::vec_c(examples_params)),
  tar_target(examples_results,
             gen_examples_dat(examples_design) |>
               fit_model(),
             pattern = map(examples_design)),
  tar_target(examples_axis_limits,
             identify_axis_limits(examples_results)),
  tar_group_by(examples_results_grouped, 
               examples_results |>
                 dplyr::mutate(.by = example_type,
                               rep = row_number()),
               example_type),
  tar_target(examples_spaghetti_plts, 
             examples_results_grouped |>
               pluck("model", 1) |> 
               make_spaghetti_plt(),
             pattern = map(examples_results_grouped),
             iteration = "list"),
  tar_target(examples_condition_plt,
             patch_plt_dat(examples_results_grouped, plt_layout, true_values, examples_axis_limits,
                           title_info_vars = c("example_type", "n_bootstraps", 
                                               "n_students", "n_schools")),
             pattern = map(examples_results_grouped),
             iteration = "list"),
  tar_target(examples_condition_plt_files,
    {path <- paste0("outputs/examples_condition_plt_", 
                     targets::tar_name(), 
                     ".png")
      ggplot2::ggsave(path, 
             examples_condition_plt, width = 8, height = 8)
      path},
    pattern = map(examples_condition_plt),
    iteration = "list",
    format = "file"),
  tar_target(examples_icc_dat, 
             make_icc_dat(examples_results_grouped),
             pattern = map(examples_results_grouped),
             iteration = "list"),
  tar_target(examples_icc_plt,
             patch_icc_dat(examples_icc_dat)),
  tar_target(examples_r2_dat, 
             make_r2_dat(examples_results_grouped),
             pattern = map(examples_results_grouped),
             iteration = "list"),
  tar_target(examples_r2_plt,
             patch_r2_dat(examples_r2_dat)),
  tar_quarto(ecls_report, "ecls-k.qmd",
             quiet = F)
)
