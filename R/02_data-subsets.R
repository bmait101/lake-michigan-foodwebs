# Prepare data subsets for TP analysis


# Load functions and compiled dataset
source(here::here("R", "00_prep.R"))
load(here("out", "data", "compiled_data.RData"))


# all data -------------------------------------------
df <- data |> 
  select(
    dataset, lake_region, year, season, depth_m, 
    compartment, species, species_group, 
    d13C = d13c_norm,  d15N = d15n, length_mm, mass_g
  ) |> 
  # assign north and south lake basins
  mutate(lake_region = case_when(
    lake_region %in% c("nw", "ne") ~ "north",
    lake_region %in% c("sw", "se") ~ "south"
  )) |>
  # assiagn baselines and consumer 
  mutate(trophic = case_when(
    compartment == "pom" ~ "b1", compartment == "macro-alga" ~ "b2", TRUE ~ "consumer"
  )) |>
  # filter(!species %in% c("slimy sculpin", "deepwater sculpin")) |>
  filter(!compartment %in% c("ichthoplankton")) |>
  rowid_to_column("ID") |> 
  drop_na(d13C, d15N, lake_region, year, season) 


# 2015 data ----------------------------------------------

df_2015 <- df |> 
  filter(dataset %in% c("csmi_2015", "nps_2015"))

# Scale 1 - pooled across lake region and seasons
df_2015_01 <- df_2015 |> 
  mutate(scale = "pooled") |>
  droplevels() |> as.data.frame()

# Scale 2 - by lake region 
df_2015_02a <- df_2015 |> 
  mutate(scale = lake_region) |>
  droplevels() |> as.data.frame()

# Scale 2 - by lake region but with pooled baselines
df_2015_02b <- df_2015 |> 
  mutate(scale = "pooled") |>
  mutate(species = paste(species, lake_region, sep = "_")) |>
  droplevels() |> as.data.frame()

# Scale 3 - by basin and season
df_2015_03a <- df_2015 |> 
  mutate(scale = paste(lake_region, season, sep = "_")) |>
  droplevels() |> as.data.frame()

# Scale 3 - by basin and season by with baselines pooled across seasons
df_2015_03b <- df_2015 |> 
  mutate(scale = lake_region) |>
  mutate(species = paste(species, season, sep = "_")) |>
  droplevels() |>  as.data.frame()

# Put in list
data_subsets <- list(
  df_2015_01,
  df_2015_02a,
  df_2015_02b,
  df_2015_03a, 
  df_2015_03b
)

save(data_subsets, file = here("out", "models", "tp", "tp_data_subs_2015.RData"))


# Individual levels

df_2015_01_ind <- df |>
  filter(dataset %in% c("csmi_2015", "nps_2015")) |>
  mutate(scale = "pooled") |>
  mutate(species = paste(ID, species, sep = "_")) |>
  droplevels() |> as.data.frame()

df_2015_02_ind <- df |>
  filter(dataset %in% c("csmi_2015", "nps_2015")) |>
  mutate(scale = lake_region) |>
  mutate(species = paste(ID, species, sep = "_")) |>
  droplevels() |>
  as.data.frame()

df_2015_03_ind <- df |>
  filter(dataset %in% c("csmi_2015", "nps_2015")) |>
  mutate(scale = paste(lake_region, season, sep = "_")) |>
  mutate(species = paste(ID, species, sep = "_")) |>
  droplevels() |>
  as.data.frame()

# Put in list
data_subsets_ind <- list(
  df_2015_01_ind,
  df_2015_02_ind,
  df_2015_03_ind
)

save(data_subsets_ind, file = here("out", "models", "tp", "tp_data_subs_2015_ind.RData"))


# Viz data
# df_2015_01_ind |>
#   ggplot(aes(d13C, d15N)) +
#   geom_point(aes(color = trophic), size = 2, alpha = .5) +
#   facet_wrap(vars(scale)) +
#   labs(color = "Trophic Group", shape = "Trophic Group")


