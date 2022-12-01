


# Summary ========================

data |> count(dataset, compartment)
data |> group_by(dataset, compartment) |> skimr::skim()


# Plot data  =======================================================

data |> 
  ggplot(aes(d13c_norm, d15n, color = dataset)) +
  # ggplot(aes(d13c_norm, d15n, color = compartment, shape = compartment)) +
  geom_point(alpha = .5, size = 2) + 
  # facet_wrap(vars(dataset)) + 
  scale_color_fish_d(option = "Scarus_quoyi")

data |> 
  # ggplot(aes(d13c, d15n, color = dataset)) +
  ggplot(aes(d13c_norm, d15n, color = compartment, shape = compartment)) +
  geom_point(alpha = .5, size = 2) + 
  facet_wrap(vars(dataset)) +
  scale_color_fish_d(option = "Scarus_quoyi")

data |> 
  filter(species != "gizzard shad") |> 
  ggplot(aes(d13c_norm, d15n)) +
  geom_point(color = "black", alpha = .5, size = 2) + 
  geom_point(
    data = data |> filter(species == "gizzard shad"),
    color = "red", alpha = .5, size = 2)

data |> 
  filter(species %in% c("dreissena")) |> 
  mutate(species_group = case_when(
    depth_m < 18 ~ "nearshore",
    depth_m >= 18 ~ "offshore"
  )) |> 
  ggplot(aes(d13c_norm, d15n, color = species_group)) +
  geom_point(alpha = .5, size = 3) + 
  facet_wrap(vars(dataset, season))


data |> 
  # filter(compartment=="fishes") |> 
  filter(species_group%in% c("yellow perch", "slimy sculpin lg","deepwater sculpin","bloater lg")) |>
  ggplot(aes(d15n, factor(depth_m)))+ geom_boxplot() + facet_wrap(vars(species_group))
