clean_ecls <- function(data) {
  data |>
    select(childid, s2_id, x12sesl, x2pubpri, x2mthetk5) |>
    mutate(private = recode_values(as.character(x2pubpri),
                                   "1: PUBLIC" ~ 0,
                                   "2: PRIVATE" ~ 1,
                                   "-1: NOT APPLICABLE" ~ NA,
                                   "-9: NOT ASCERTAINED" ~ NA),
           schoolid = s2_id,
           ses = x12sesl,
           mathscore = x2mthetk5,
           .keep = "unused") |>
    drop_na() |>
    # removing schools with less than 10 students
    filter_out(.by = schoolid,
               length(unique(childid)) < 10)
}