# Prepare data subsets for TP analysis


# Load functions and compiled dataset
source(here::here("R", "00_prep.R"))
load(here("out", "data", "compiled_data.RData"))



# Assign factors =========================================================

data <- data |> 
  select(-date) |> 
  mutate(
    depth_g = case_when(
      depth_m <= 30 ~ "photic", 
      depth_m > 30 & depth_m <= 90 ~ "mid-depth", 
      depth_m > 90 ~ "profundal", 
      TRUE ~ "offshore"
    )
  ) |> 
  
  mutate(
    dataset = factor(
      dataset, 
      levels = c(
        "uwm_2002_2003","uwm_2010_2011","kornis_2014",
        "csmi_2015","nps_2015","glft_2016","roth_2019"
      )), 
    compartment = factor(
      compartment, levels = c(
        "pom", "macro-alga","benthic inverts",
        "dreissenids","ichthoplankton","zooplankton","fishes"
      )), 
    lake_region = factor(
      lake_region, 
      levels = c("nw", "ne", "sw", "se")), 
    season = factor(season, levels = c(1,2,3,4)), 
    depth_g = factor(depth_g, 
                     levels = c("photic","mid-depth","profundal"))
  ) |> 
  mutate(across(where(is.character), as.factor)) |> 
  mutate(across(c(season), as.numeric)) 




# all data -------------------------------------------
df_all <- data |> 
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
  # assign baselines and consumer 
  mutate(trophic = case_when(
    compartment == "pom" ~ "b1", compartment == "macro-alga" ~ "b2", TRUE ~ "consumer"
  )) |>
  # filter(!species %in% c("slimy sculpin", "deepwater sculpin")) |>
  filter(!compartment %in% c("ichthoplankton")) |>
  rowid_to_column("ID") |> 
  drop_na(d13C, d15N, lake_region, year, season) 

# Year subsets ----------------------------------------------


df_2015 <- df_all |> 
  filter(dataset %in% c("csmi_2015", "nps_2015"))


# Summary ========================

vis_dat(df_2015)
vis_miss(df_2015)

df_2015 |>
  count(species)|> print(n=Inf)

df_2015 |>
  count(comprtment, species)

df_2015 |>
  count(dataset, compartment, species, lake_region, season) |> 
  pivot_wider(names_from = c(lake_region, season), values_from = n, values_fill = 0) |> 
  print(n=Inf)


df_2015 |> 
  filter(! compartment %in% c("pom", "macro-alga")) |> 
  filter(is.na(length_mm)) |> 
  group_by(compartment) |> 
  summarise(n = n()) |> 
  mutate(freq = n / sum(n)) |> 
  print(n=Inf)

df_2015 |> 
  filter(! compartment %in% c("pom", "macro-alga")) |> 
  filter(!is.na(mass_g)) |> 
  group_by(compartment, species) |> 
  summarise(mean_length = mean(length_mm), 
            mean_mass = mean(mass_g))

# df_14_16 <- df_all |>
#   filter(dataset %in% c("csmi_2015", "nps_2015", "kornis_2014", "glft_2016"))
# 
# df_2019 <- df_all |> 
#   filter(dataset %in% c("csmi_2015", "nps_2015"))


# all data ----------------------------------------------

data_subs_all <- list(
  # Scale 1 - pooled across lake region and seasons
  df_all |> 
    mutate(scale = "pooled") |>
    droplevels() |> as.data.frame(),
  # Scale 2 - by lake region 
  df_all |> 
    mutate(scale = lake_region) |>
    droplevels() |> as.data.frame(),
  # Scale 2 - by lake region but with pooled baselines
  df_all |> 
    mutate(scale = "pooled") |>
    mutate(species = paste(species, lake_region, sep = "_")) |>
    droplevels() |> as.data.frame(),
  # Scale 3 - by basin and season
  df_all |> 
    mutate(scale = paste(lake_region, season, sep = "_")) |>
    droplevels() |> as.data.frame(),
  # Scale 3 - by basin and season by with baselines pooled across seasons
  df_all |> 
    mutate(scale = lake_region) |>
    mutate(species = paste(species, season, sep = "_")) |>
    droplevels() |>  as.data.frame(), 
  # Scale 4 - by basin and season by with baselines pooled across seasons
  df_all |> 
    mutate(scale = paste(lake_region, season, sep = "_")) |>
    mutate(species = paste(species, year, sep = "_")) |>
    droplevels() |>  as.data.frame()
)

names(data_subs_all) <- c(
  "scale01", "scale02a", "scale02b", "scale03a", "scale03b", "scale04"
)

save(data_subs_all, file = here("out", "data", "data_subs_all.RData"))

# 2015 data ----------------------------------------------

data_subs_15 <- list(
  # Scale 1 - pooled across lake region and seasons
  df_2015 |> 
    mutate(scale = "pooled") |>
    droplevels() |> as.data.frame(),
  # Scale 2 - by lake region 
  df_2015 |> 
    mutate(scale = lake_region) |>
    droplevels() |> as.data.frame(),
  # Scale 2 - by lake region but with pooled baselines
  df_2015 |> 
    mutate(scale = "pooled") |>
    mutate(species = paste(species, lake_region, sep = "_")) |>
    droplevels() |> as.data.frame(),
  # Scale 3 - by basin and season
  df_2015 |> 
    mutate(scale = paste(lake_region, season, sep = "_")) |>
    droplevels() |> as.data.frame(),
  # Scale 3 - by basin and season by with baselines pooled across seasons
  df_2015 |> 
    mutate(scale = lake_region) |>
    mutate(species = paste(species, season, sep = "_")) |>
    droplevels() |>  as.data.frame()
  )

names(data_subs_15) <- c(
  "scale01", "scale02a", "scale02b", "scale03a", "scale03b"
  )

save(data_subs_15, file = here("out", "data", "data_subs_2015.RData"))

# Individual levels
data_subs_15_ind <- list(
  df_2015 |>
    mutate(scale = "pooled") |>
    mutate(species = paste(ID, species, sep = "_")) |>
    droplevels() |> 
    as.data.frame(),
  df_2015 |>
    mutate(scale = lake_region) |>
    mutate(species = paste(ID, species, sep = "_")) |>
    droplevels() |>
    as.data.frame(),
  df_2015 |>
    mutate(scale = paste(lake_region, season, sep = "_")) |>
    mutate(species = paste(ID, species, sep = "_")) |>
    droplevels() |>
    as.data.frame()
  )

names(data_subs_15_ind) <- c(
  "scale01_ind", "scale02_ind", "scale03_ind"
)

save(data_subs_15_ind, file = here("out", "data", "data_subs_2015_ind.RData"))



# 2014-2016 data ----------------------------------------------

# data_subs_14_16 <- list(
#   # Scale 1 - pooled across lake region and seasons and years
#   df_14_16 |> 
#     mutate(scale = "pooled") |>
#     droplevels() |> 
#     as.data.frame(),
#   # Scale 2 - by lake region but with pooled baselines from 2015
#   df_14_16 |> 
#     mutate(scale = "pooled") |>
#     mutate(species = paste(species, lake_region, sep = "_")) |>
#     droplevels() |> 
#     as.data.frame(),
#   # Scale 3 - by region and season by with baselines from 2015
#   df_14_16 |> 
#     mutate(scale = "pooled") |>
#     mutate(species = paste(species, season, lake_region, sep = "_")) |>
#     droplevels() |> 
#     as.data.frame(),
#   # Scale 4 - by basin and season and year with baselines from 2015
#   df_14_16 |> 
#     mutate(scale = "pooled") |>
#     mutate(species = paste(species, season, lake_region, year, sep = "_")) |>
#     droplevels() |>  
#     as.data.frame(),
#   # Scale ind - by basin and season and year with baselines from 2015
#   df_14_16 |> 
#     mutate(scale = "pooled") |>
#     mutate(species = paste(ID, species, season, lake_region, year, sep = "_")) |>
#     droplevels() |>  
#     as.data.frame()
#   )
# 
# names(data_subs_14_16) <- c(
#   "scale01", "scale02", "scale03", "scale04", "scale05"
#   )
# 
# save(data_subs_14_16, file = here("out", "data", "data_subs_14_16.RData"))


# Viz data ----------------------------
data_subs_15$scale01 |>
  ggplot(aes(d13C, d15N)) +
  geom_point(aes(color = trophic), size = 2, alpha = .5) +
  facet_wrap(vars(scale)) +
  labs(color = "Trophic Group", shape = "Trophic Group")


