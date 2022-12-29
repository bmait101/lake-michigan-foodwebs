
# Check for outliter, etc. 


# Summary ========================

vis_dat(data)
vis_miss(data)

data |> 
  filter(dataset == "glft_2016") |> 
  count(compartment, species, species_group) |> print(n=Inf)

data |> group_by(dataset, compartment) |> skimr::skim()

data |> filter(is.na(length_mm)) |> count(compartment, species) |> print(n=Inf)



## Outliers  ==================================================================

data |>
  filter(d13c < -10) |>
  filter(d13c > -40) |>
  ggplot(aes(d13c)) + geom_boxplot()
data |> ggplot(aes(d15n)) + geom_boxplot()
data |> ggplot(aes(cn)) + geom_boxplot()
data |> ggplot(aes(length_mm)) + geom_boxplot()
data |> ggplot(aes(mass_g)) + geom_boxplot()

data <- data |> 
  filter(d13c < -10) |> 
  filter(d13c > -40) |> 
  filter(cn < 30)



