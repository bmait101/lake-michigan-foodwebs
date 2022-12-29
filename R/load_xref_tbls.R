
# Xref table

xref_ports <- 
  read_csv(here("data", "xref-sites-ports-regions.csv")) |> 
  cleans_names_and_caps()

xref_compartments <- 
  read_csv(here("data", "xref_taxa_compartment.csv")) |> 
  cleans_names_and_caps()

# load scientific names
xref_sci_names <- 
  read.csv(here("data", "xref_fish_species_names.csv")) |> 
  mutate(common_name = str_to_lower(common_name))
