get_warnings <- function(model) {
  warnings <- model |> pluck("optinfo") |> pluck("warnings")
  
  return(
    ifelse(length(warnings) == 0, NA_character_, warnings)
  )
}