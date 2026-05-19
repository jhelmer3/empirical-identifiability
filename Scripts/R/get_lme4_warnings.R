get_lme4_warnings <- function(model) {
  warnings <- model |> pluck("optinfo") |> pluck("conv") |> 
    pluck("lme4") |> pluck("messages") |> pluck(1)
  
  return(
    ifelse(is_null(warnings), NA_character_, warnings)
  )
}