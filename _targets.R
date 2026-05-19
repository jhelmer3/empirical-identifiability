
library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_option_set(
  packages = c("tidyverse", "patchwork", "gt"),
  controller = crew_controller_local(workers = 8),
  format = "qs"
)

tar_source(here::here("Scripts", "R"))

n_bootstraps <- 24

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
  tar_target(model_terms, full_conditional_model |> broom.mixed::tidy() |> pull(term)),
  tar_target(plt_layout, make_plt_layout(model_terms)),
  tar_target(param_set, init_params(n_bootstraps)),
  tar_target(params, define_params(param_set)),
  tar_rep(results, ecls_dat |> 
            bootstrap_data(params) |>
            fit_model(), 
          reps = n_bootstraps / 8, 
          batches = 8),
  tar_group_by(results_grouped, results |> 
                 dplyr::mutate(.by = condition_id,
                               rep = row_number()), 
               condition_id),
  tar_target(condition_plt, 
             patch_plt_dat(results_grouped, plt_layout),
             pattern = map(results_grouped),
             iteration = "list"),
  tar_quarto(ecls_report, "ecls-k.qmd",
             quiet = F)
)
