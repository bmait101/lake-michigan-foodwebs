# Helper functions

cleans_names_and_caps <- function(data) {
  data |> 
    janitor::clean_names() |> 
    mutate_if(is.character, str_to_lower)
}