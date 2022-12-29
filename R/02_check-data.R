
# Libraries, xrefs, and helpers
source(here::here("R", "00_prep.R"))

# Data
load(here("out", "data", "compiled_data.RData"))


# Summary ========================

vis_dat(data)
vis_miss(data)

data |> 
  filter(dataset == "glft_2016") |> 
  count(compartment, species, species_group) |> print(n=Inf)

data |> group_by(dataset, compartment) |> skimr::skim()

data |> filter(is.na(length_mm)) |> count(compartment, species) |> print(n=Inf)



# Plots  =======================================================


