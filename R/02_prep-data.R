# Prepare data TP and alpha estimation
# 1. Subset data for analysis from compiled dataset
# 2. Remove outliers
# 3. Convert body sizes for fishes, add lit values for inverts

## Data =======================================================================


# Load functions and compiled dataset
source(here::here("R", "00_prep.R"))
# load(here("out", "data", "compiled_data.RData"))
load(here("out", "data", "compiled_data_v3.RData"))


### Initial prep  =============================================================

# 2015 only and no green bay
df <- data |> 
  filter(dataset %in% c("csmi_2015", "nps_2015")) |> 
  filter(!compartment %in% c("ichthoplankton")) 

# add a basin category
df <- df |> 
  mutate(
    basin = case_when(
      lake_region %in% c("nw", "ne") ~ "north",
      lake_region %in% c("sw", "se") ~ "south")) |> 
  relocate(basin, .before = lake_region)


# assign baselines and consumer 
df <- df |> 
  mutate(
    trophic = case_when(
      compartment == "pom" ~ "b1", 
      compartment == "benthic algae" ~ "b2",
      TRUE ~ "consumer"
    )) 

# add row ids and drop missing data
df <- df |> 
  # select(-sample_id, -date) |> 
  # add unique row IDs
  rowid_to_column("ID") |> 
  # drop any observations missing isotope data or groupings
  drop_na(d13c_norm, d15n, lake_region, season) 


## SI Outliers  ===============================================================

# df |> ggplot(aes(d13c)) + geom_boxplot()  # 1 POM outlier
# df |> ggplot(aes(d13c_norm)) + geom_boxplot()  # 4 zoops
# df |> filter(compartment=="zooplankton") |>   # outliers for cn values
#   ggplot(aes(cn)) +
#   geom_boxplot()
# df |> ggplot(aes(d15n)) + geom_boxplot()  # d15N looks good
# df |> ggplot(aes(cn)) + geom_boxplot()  # large spread but okay


# remove extreme outliers
df |> filter(d13c_norm < -40)
df |> filter(d13c_norm > -12)

df <- filter(df, ! d13c_norm < -40)
df <- filter(df, ! d13c_norm > -12)

# Check plot
# df |>
#   filter(compartment %in% c("pom", "benthic algae")) |>
#   ggplot(aes(d13c_norm, d15n)) +
#   geom_point(aes(color = species, shape = factor(season)), size = 3)+
#   facet_wrap(vars(lake_region))



## Body size   ========================================================

### Fishes ----------------------------------------

# Check missing data size measurements
# df |>
#   vis_miss()
# 57.2% have measurements

# Check missing data for fishes
# df |>
#   filter(compartment %in% c("fishes")) |>
#   vis_miss()
# 6% missing lengths
# 62% missing mass


# load/join log a and b parameters from L-W regressions (Bayes_LWR_Model.R)
df_lw_params <- read_rds(here("out", "tbls", "body_mass_params.rds"))
df_lw_params_v2 <- read_rds(here("out", "tbls", "body_mass_params_v2.rds")) |> 
  mutate(across(c(log_a, b), as.numeric))

df <- df |> 
  left_join(df_lw_params, by = "sci_name") |> 
  mutate(across(c(sci_name), as.factor)) 

# Convert length to mass
df <- 
  df |> 
  mutate(length_cm = length_mm / 10) |>  # need cm for conversion
  mutate(mass_g = case_when(
    is.na(mass_g) & (!is.na(log_a)) ~ 10^(log_a + b * (log10(length_cm))), 
    TRUE ~ mass_g
  )) |> 
  select(-log_a, -b, -length_cm)

# check it
# df |>
#   filter(compartment %in% c("fishes")) |>
#   vis_miss()
# 6% missing lengths
# 6% missing mass

rm(df_lw_params)
rm(df_lw_params_v2)

### Inverts ------------------------------------------------------

# Assign body mass average values based on literature
# df |> 
#   filter(compartment %in% c("dreissenids","zooplankton","benthic inverts")) |> 
#   count(species)

df <- df |> 
  mutate(
    mass_g = case_when(
      species == "crayfish" ~ 4.6, # Anderson and Simon 2012; male/female/juve average
      species == "dreissena" ~ .035, # Nalepa et al. 2010, converted from AFDM (0.2:1), Evan et al. 2022
      species == "amphipod" ~ .005, # Calveletto et al. 1996; DM to WM (0.2:1), Evan et al. 2022
      species == "oligochaete" ~ .001, # Jimenez et al 2001; DM to WM (0.2:1), Evan et al. 2022
      species == "chironomids" ~ .002, # Jimenez et al 2001; DM to WM (0.2:1), Evan et al. 2022

      species == "hydrachnidiae" ~ .0012,  # Bootsma data 2010
      species == "sphaeriidae" ~ .0003,  # Strayer & Malcom 2018
      # species == "gastropoda" ~ .002,   
      # species == "trichoptera" ~ .002,  
      
      species == "mysis" ~ .005, # Evan et al. 2022
      species == "hemimysis" ~ .001, # Marty et al. 2010
      species == "bythotrephes" ~ .0002,  # Evan et al. 2022
      species == "zooplankton240" ~ .00001,  # Evan et al. 2022
      species == "zooplankton153" ~ .000001,  # Evan et al. 2022
      species == "zooplankton63" ~ .0000001,  # Evan et al. 2022
      
      # compartment == "zooplankton" ~ .00001,
      
      species == "isopod" ~ 0.001, # Calvaletto (convert DM to WW)
      species == "hydracarina" ~ .002, # Meyer 1989
      species == "leech" ~ .2, # Meyer 1989

      TRUE ~ mass_g
    )
  ) 


# Check missing body size for all consumers now:
# df |>
#   filter(! compartment %in% c("pom", "benthic algae")) |>
#   filter(is.na(mass_g)) |>
#   count(compartment) |>
#   print(n=100)
# 82 fishes missing mass data

# which species are missing body size?
# df |>
#   filter(compartment %in% c("fishes")) |>
#   filter(is.na(mass_g)) |>
#   count(species_group) |>
#   print(n=100)


### Check -------

# Check missing again for consumers
# vis_miss(df)
# 46% missing mass to 11% 


