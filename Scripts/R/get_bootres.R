get_bootres <- function(params) {
  print(params)
  list2env(params, envir = environment())
  
  frm <- mathscore ~ ses * private + (ses | schoolid)
  
  ecls_dat |>
    filter(schoolid %in% sample(schoolid, n_schools)) |>
    filter(.by = schoolid,
           childid %in% sample(childid, n_students)) |>
    # should this take into account the clustering more?
    modelr::bootstrap(n_bootstraps, id = "id") |>
    mutate(res = map(strap, 
                      safely(\(strap) lme4::lmer(frm,
                                           data = strap))),
           errors = map(res, "error"),
           model = map(res, "result"),
           tidy = imap(model, \(model, idx) model |>
                         broom.mixed::tidy() |>
                         mutate(iter = idx)),
           opt_warnings = map_chr(model, get_warnings),
           lme4_warnings = map_chr(model, get_lme4_warnings),
           params = tibble(params) |>
             rename_with(\(name) glue::glue("param_{name}"))) |>
    unnest(params)
}

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




