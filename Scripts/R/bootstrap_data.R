
bootstrap_data <- function(ecls_dat, replication) {
  replication |>
    mutate(
      bootstrap = map2(
        n_students, n_schools, 
        \(n_students, n_schools) ecls_dat |>
          filter(schoolid %in% sample(schoolid, n_schools)) |>
          filter(.by = schoolid,
                 childid %in% sample(childid, n_students)) 
      )
    )
  
  # bootstrap <- ecls_dat |>
  #   filter(schoolid %in% sample(schoolid, param_replication$n_schools)) |>
  #   filter(.by = schoolid,
  #          childid %in% sample(childid, param_replication$n_students)) |>
  #   nest(bootstrap = everything())
# 
#   param_replication |>
#     bind_cols(bootstrap)
}

# tar_read(condition_params) |> head(1)
# 
# 
# 
# 
# tibble(cluster = c(1, 2, 3),
#        n_cluster = c(2, 3, 4)) |>
#   uncount(n_cluster) |>
#   mutate(unit = row_number()) |>
#   slice_sample(n = 2)


# get_bootres <- function(ecls_dat, condition_params) {
#   
#   frm <- mathscore ~ ses * private + (ses | schoolid)
#   
#   ecls_dat |>
#     filter(schoolid %in% sample(schoolid, condition_params$n_schools)) |>
#     filter(.by = schoolid,
#            childid %in% sample(childid, condition_params$n_students)) |>
#     # should this take into account the clustering more?
#     modelr::bootstrap(condition_params$n_bootstraps, id = "id") |>
#     mutate(res = map(strap, 
#                      safely(\(strap) lme4::lmer(frm, data = strap),
#                             otherwise = NA_real_)),
#            model = map(res, "result"),
#            error = map_chr(res, \(res) pluck(res, "error", "message", .default = NA)),
#            tidy = imap(model, \(model, idx) model |>
#                          broom.mixed::tidy() |>
#                          mutate(iter = idx)),
#            opt_warnings = map_chr(model, get_warnings),
#            lme4_warnings = map_chr(model, get_lme4_warnings),
#            params = tibble(params) |>
#              rename_with(\(name) glue::glue("param_{name}"))) |>
#     unnest(params)
# }


# get_bootres <- function(ecls_dat, condition_params) {
# 
#   frm <- mathscore ~ ses * private + (ses | schoolid)
#   
#   ecls_dat |>
#     filter(schoolid %in% sample(schoolid, condition_params$n_schools)) |>
#     filter(.by = schoolid,
#            childid %in% sample(childid, condition_params$n_students)) |>
#     # should this take into account the clustering more?
#     modelr::bootstrap(condition_params$n_bootstraps, id = "id") |>
#     mutate(res = map(strap, 
#                       safely(\(strap) lme4::lmer(frm, data = strap),
#                              otherwise = NA_real_)),
#            model = map(res, "result"),
#            error = map_chr(res, \(res) pluck(res, "error", "message", .default = NA)),
#            tidy = imap(model, \(model, idx) model |>
#                          broom.mixed::tidy() |>
#                          mutate(iter = idx)),
#            opt_warnings = map_chr(model, get_warnings),
#            lme4_warnings = map_chr(model, get_lme4_warnings),
#            params = tibble(params) |>
#              rename_with(\(name) glue::glue("param_{name}"))) |>
#     unnest(params)
# }

# d <- tibble(n_bootstraps = 10,
#             n_students = 4,
#             n_schools = 10) |>
#   get_bootres()

# get_bootres <- function(params) {
#   frm <- mathscore ~ ses * private + (ses | schoolid)
#   
#   ecls_dat |>
#     filter(schoolid %in% sample(schoolid, n_schools)) |>
#     filter(.by = schoolid,
#            childid %in% sample(childid, n_students)) |>
#     # should this take into account the clustering more?
#     modelr::bootstrap(n_bootstraps, id = "id") |>
#     mutate(model = map(strap, 
#                        \(strap) lme4::lmer(frm,
#                                            data = strap)),
#            tidy = imap(model, \(model, idx) model |>
#                          broom.mixed::tidy() |>
#                          mutate(iter = idx)),
#            opt_warnings = map_chr(model, get_warnings),
#            lme4_warnings = map_chr(model, get_lme4_warnings))
# }
# 
# tibble(x = list("a", 10, 100),
#        res = map(x, safely(\(x) log(x),
#                            otherwise = NA_real_))) |>
#   mutate(result = map_dbl(res, "result"),
#          error = map_chr(res, \(res) pluck(res, "error", "message", .default = NA)))



