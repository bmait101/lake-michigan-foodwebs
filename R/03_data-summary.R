source(here::here("R", "00_prep.R"))
source(here::here("R", "02_prep-data.R"))

## Summary ========================

summary(df)

skim(df)

# Viz data class structure
vis_dat(df)

# Visualize missing data
vis_miss(df)

# Counts by species and compartment
df |> count(species)|> arrange(n) |>  print(n=Inf)
df |> count(compartment, species)


# List of ports for map making
df |> 
  distinct(port) |> 
  # print(n=Inf) |> 
  write_csv(here("out", "tbls", "list-of-ports_v2.csv"))


# List of iso samples for appendix
df |>
  count(dataset, compartment, sci_name, lake_region, season) |>
  pivot_wider(names_from = c(lake_region, season), values_from = n, values_fill = 0) |>
  print(n=Inf) |> 
  write_csv(here("out", "tbls", "list-of-isotope-samples.csv"))

# Body size summary
df |> 
  select(compartment, sci_name, length_mm, mass_g) |> 
  group_by(compartment, sci_name) |> 
  summarise(mean_length = mean(length_mm, na.rm = TRUE), 
            mean_mass = mean(mass_g, na.rm = TRUE)) |> 
  print(n=Inf) |> 
  write_csv(here("out", "tbls", "body-size-summary.csv"))

# Baseline summary stats
df  |> 
  filter(species %in% c("pom", "benthic algae")) |> 
  group_by(species) |> 
  summarise(
    n = n(), 
    `mean d13c` = mean(d13c_norm, na.rm = TRUE), 
    `sd d13c` = sd(d13c_norm, na.rm = TRUE), 
    `mean d15n` = mean(d15n, na.rm = TRUE),
    `sd d15n` = sd(d15n, na.rm = TRUE)
  )

df  |> 
  filter(species %in% c("pom", "benthic algae")) |> 
  select(species, lake_region, season, d13c_norm, d15n) |> 
  group_by(lake_region, season, species) |> 
  summarise(
    n = n(), 
    `mean d13c` = mean(d13c_norm, na.rm = TRUE), 
    `sd d13c` = sd(d13c_norm, na.rm = TRUE), 
    `mean d15n` = mean(d15n, na.rm = TRUE),
    `sd d15n` = sd(d15n, na.rm = TRUE)
  )


## Plot data --------------

df |> count(species) |> print(n=Inf)
df |> count(compartment) |> print(n=Inf)

df |> 
  ggplot(aes(d13c_norm, d15n, color = trophic)) + 
  facet_wrap(vars(season)) + 
  geom_point() 

# summary plots with isotope means
df |> 
  group_by(species) |> 
  summarise(meanC = mean(d13c_norm), meanN = mean(d15n), 
            sdC = sd(d13c_norm), sdN = sd(d15n)) |> 
  ggplot(aes(meanC, meanN)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = meanN - sdN, ymax = meanN + sdN)) +
  geom_errorbarh(aes(xmin = meanC - sdC, xmax = meanC + sdC)) +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
  theme_bw()


df |> 
  filter(species%in%c("zooplankton63","zooplankton153","zooplankton240")) |> 
  ggplot(aes(d13c_norm, d15n, color = species)) + 
  geom_point(size = 3)

df |> 
  filter(compartment%in%c("pom","benthic algae")) |> 
  ggplot(aes(d13c_norm, d15n, color = species)) + 
  geom_point(size = 3) 

df |> 
  # filter(compartment%in%c("fishes")) |>
  filter(compartment%in%c("zooplankton","benthic inverts")) |>
  ggplot(aes(log(mass_g), d15n, color = species, shape = depth_g)) + 
  geom_jitter(size = 2)


df_plot <- df |> 
  # filter(!species%in%c("zooplankton63","zooplankton240")) |> 
  mutate(plot_groups = case_when(
    species == "pom" ~ "POM", 
    species == "benthic algae" ~ "Benthic Algae", 
    species == "dreissena" ~ "Dreissenids", 
    species == "zooplankton153" ~ "Zooplankton", 
    species == "zooplankton240" ~ "Zooplankton",
    species == "zooplankton63" ~ "Zooplankton",
    species == "amphipod" ~ "Amphipod",
    species == "hemimysis" ~ "Hemimysis",
    species == "bythotrephes" ~ "Bythotrephes",
    species == "mysis" ~ "Mysis",
    species == "alewife" ~ "Alewife",
    species == "round goby" ~ "Round Goby",
    species == "bloater" ~ "Bloater",
    species == "rainbow smelt" ~ "Rainbow Smelt",
    species == "deepwater sculpin" ~ "Deepwater Sculpin",
    species == "slimy sculpin" ~ "Slimy Sculpin",
    species == "lake trout" ~ "Lake Trout", 
    species == "brown trout" ~ "Brown Trout", 
    species == "chinook salmon" ~ "Chinook Salmon", 
    species == "coho salmon" ~ "Coho Salmon", 
    species == "burbot" ~ "Burbot"
  )) |> 
  filter(!is.na(plot_groups)) |>
  mutate(plot_groups = factor(plot_groups, levels = c(
    "POM", "Benthic Algae", "Dreissenids", "Zooplankton",
    "Amphipod",
    "Hemimysis",
    "Bythotrephes",
    "Mysis",
    "Alewife",
    "Round Goby",
    "Bloater",
    "Rainbow Smelt",
    "Deepwater Sculpin",
    "Slimy Sculpin",
    "Brown Trout", "Lake Trout", "Chinook Salmon", "Coho Salmon", "Burbot"
  ))) 

df_plot |> count(species) |> print(n=Inf)
df_plot |> count(compartment)

df_plot |> 
  # filter(trophic %in% c("b1", "b2")) |> 
  # filter(compartment %in% c("benthic inverts","dreissenids","zooplankton")) |>
  # filter(compartment %in% c("benthic inverts")) |>
  ggplot(aes(d13c_norm, d15n, color = plot_groups, shape = plot_groups)) + 
  # facet_grid(row=vars(lake_region), cols=vars(season)) + 
  geom_point(size = 3) + 
  scale_y_continuous(limits = c(-6,20)) + 
  scale_x_continuous(limits = c(-33,-13)) + 
  scale_shape_manual(values = c(
    3,3,1,2,20,20,20,20,20,20,20,20,20,20,5,5,5,5, 5
  )) + 
  scale_color_manual(values = c(
    "blue","green","green4","lightblue","red4","gold","orange",
    "red","skyblue","green3","blue","purple","pink","darkblue", 
    "cyan", "cyan1","cyan2","cyan3","cyan3"
    )) + 
  theme_clean()






df |> 
  # filter(trophic %in% c("b1", "b2")) |> 
  filter(compartment %in% c("benthic inverts","dreissenids")) |>
  # filter(compartment %in% c("benthic inverts")) |>
  ggplot(aes(d13c_norm, d15n, color = species, shape = species)) + 
  facet_grid(row=vars(lake_region), cols=vars(season)) +
  geom_point(size = 3) + 
  scale_y_continuous(limits = c(-6,20)) + 
  scale_x_continuous(limits = c(-33,-13)) 

df |> 
  ggplot(aes(depth_m, d13c_norm)) +
  geom_point(aes(color = trophic), size = 2, alpha = .5) + 
  facet_wrap(vars(species), scales = 'free') + 
  geom_smooth(method = "lm")
