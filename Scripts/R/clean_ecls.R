clean_ecls <- function(data) {
  data |>
    as_tibble() |>
    select(childid, s2_id, x12sesl, # x2pubpri, 
           x2locale, x2mthetk5) |>
    mutate(
      # private = recode_values(as.character(x2pubpri),
      #                         "1: PUBLIC" ~ 0,
      #                         "2: PRIVATE" ~ 1,
      #                         "-1: NOT APPLICABLE" ~ NA,
      #                         "-9: NOT ASCERTAINED" ~ NA),
      rural = recode_values(as.character(x2locale),
                              "4: RURAL (41, 42, 43)" ~ 1,
                              "-1: NOT APPLICABLE" ~ NA,
                              "-9: NOT ASCERTAINED" ~ NA,
                            default = 0),
      schoolid = s2_id,
      ses = x12sesl,
      mathscore = x2mthetk5,
      .keep = "unused") |>
    drop_na() |>
    # removing schools with less than 10 students
    filter_out(.by = schoolid,
               length(unique(childid)) < 10)
}

