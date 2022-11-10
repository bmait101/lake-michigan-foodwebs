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
    relocate(c(lake_region, port, site_code, depth_m), .after = season) |> 
    relocate(cn, .after = d13c) |> 
    relocate(c(length_mm, mass_g), .after = num_ind) |> 
    relocate(c(compartment, taxon, taxon_specific, taxon_code), .after = depth_m)
}
