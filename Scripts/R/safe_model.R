
safe_model <- function(data) {
  safely(
    \(data) lme4::lmer(mathscore ~ ses * private + (ses | schoolid), data),
    otherwise = NA_real_
  )(data)
}
