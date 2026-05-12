
library(tidyverse)
library(haven)

file.exists(here::here("Data", "TIMSS", "bspusam8.rdata"))

achievement <- rio::import(here::here("Data", "TIMSS", "bsausam8.rdata"))
student_context <- rio::import(here::here("Data", "TIMSS", "bsgusam8.rdata"))
school_context <- rio::import(here::here("Data", "TIMSS", "bcgusam8.rdata"))

dat <- student_context |> select(school_id = IDSCHOOL, class_id = IDCLASS, student_id = IDSTUD,
            starts_with("BSMMAT0"), age = BSDAGE, sex = ITSEX, student_likes_math = BSDGSLM) |>
  rename_with(~ paste0("math_pv_", parse_number(.x)), starts_with("BSMMAT0")) |>
  left_join(school_context |>
              select(school_id = IDSCHOOL, school_socioeconomic = BCDGSBC),
            by = "school_id") |>
  mutate(across(everything(), ~ case_when(.x == 9 ~ NA, 
                                          .default = .x)))  |>
  drop_na()

achievement$IDSCHOOL |> unique() |> length()
achievement$IDCLASS |> unique() |> length()
achievement$IDSTUD |> unique() |> length()

summary(dat)

lme4::lmer(math_pv_1 ~ age + sex + student_likes_math + school_socioeconomic + (1 | school_id),
           data = dat) |>
  sjPlot::tab_model()

dat |>
  lme4::lmer(math_pv_1 ~ age + sex + student_likes_math + school_socioeconomic + (age + sex + student_likes_math | school_id),
             data = _) |>
  sjPlot::tab_model()

dat |>
  filter(school_id %in% sample(school_id, 10)) |>
  filter(student_id %in% sample(student_id, 100)) |>
  lme4::lmer(math_pv_1 ~ age + sex + student_likes_math + school_socioeconomic + (age + sex + student_likes_math | school_id),
             data = _) |>
  summary()
