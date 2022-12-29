# Helper functions

cleans_names_and_caps <- function(data) {
  data |> 
    janitor::clean_names() |> 
    mutate_if(is.character, str_to_lower)
}


relocate_columns <- function(data) {
  data |> 
    relocate(dataset, .before = sample_id) |> 
    relocate(c(year, season), .after = date) |> 
    relocate(c(lake_region, port, depth_m), .after = season) |> 
    relocate(c(compartment, species, species_group, num_ind), .after = depth_m) |> 
    relocate(c(length_mm, mass_g, d15n, d13c, cn), .after = num_ind)
}


