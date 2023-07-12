
# Process TP models

source(here::here("R", "00_prep.R"))


# Data =========================================================================

## Data subsets used for TP models
load(file = here("out", "data", "data_subs_v3.RData"))
load(file = here("out", "data", "data_subs_ind_v3.RData"))

## TP model objects
load(file = here("out", "models", "tp", "tp_mods_v2.RData"))
load(file = here("out", "models", "tp", "tp_mods_ind_v2.RData"))
load(file = here("out", "models", "tp", "tp_mods_ind_v2_pooled.RData"))

tp_mods_ind <- c(tp_mods_ind, tp_mods_ind_pooled)

tp_mods_ind_all <- 
  c(
  tp_mods_ind[1], 
  tp_mods_ind_pooled[1], 
  tp_mods_ind_pooled[2], 
  tp_mods_ind_pooled[3], 
  tp_mods_ind_pooled[4], 
  tp_mods_ind[2], 
  tp_mods_ind[3], 
  tp_mods_ind[4], 
  tp_mods_ind[5]
  
  )

# Extract TP data from Parallel ===========================

# Function to extract tp data from parallel and name cols
summarize_TP_data <- function(tp_mod_obj){
  out <- fromParallelTP(tp_mod_obj, get = "summary")
  colnames(out) <- c(
    'model','scale',"species",
    'TP_lower','TP_upper','TP_median','TP_mode',
    'Alpha_lower','Alpha_upper','Alpha_median','Alpha_mode'
  )
  out
}

# Summarize TP data
tp_data <- map(tp_mods, summarize_TP_data)
tp_data_ind <- map(tp_mods_ind_all, summarize_TP_data)

# Check output
str(tp_data)
str(tp_data_ind)


# Data for HBMs models  ========================================================

# Species-levels TP data needs to be linked to average body mass
# Ind models do not need to be linked back to original data, but formatted

# Species-levels -------------------------------------------

# Function to summarize body size, merge TP data, and log transform body mass
avg_mass_and_merge <- function(data, tp_data) {
  out <- data |> 
    filter(trophic == "consumer") %>%
    select(species, scale, mass_g) %>%
    group_by(species, scale) %>%
    summarise(mass_g = mean(mass_g, na.rm = TRUE), .groups = 'drop') |> 
    drop_na(mass_g) |>
    left_join(tp_data, by = c("species", "scale")) |> 
    mutate(log_mass = log(mass_g)) 
  out
}

# Map over data and tp data
reg_mod_data <- map2(data_subs, tp_data, avg_mass_and_merge)

# Check output
reg_mod_data[[2]]


# Individual-levels -------------------------------------------

# Function to format individual-level TP model data
prep_ind_mod_data <- function(data) {
  out <- data |> 
    select(-model) |> 
    relocate(species, .before = scale) |> 
    as_tibble()
  out
}

reg_mod_data_ind <- map(tp_data_ind, prep_ind_mod_data)

names(reg_mod_data_ind) <- c(
  "p1_id","p2_id", "p3_id", "p4_id", "p5_id",
  "s1_id", "s2_id","s3_id","s4_id"
)
# Check output
reg_mod_data_ind[[6]]


# Process the combined variables in each list element ==========================

# Combine species and inv data
reg_mod_data_all <- c(reg_mod_data, reg_mod_data_ind)

# Check it
str(reg_mod_data_all)

# Function: assign species weights for individual-level models
assign_spp_weight <- function(data) {
  data |> 
    # separate(species, into = c("ID", "species"), sep = "_") |> 
    group_by(species) |> 
    mutate(count = n()) |>
    ungroup() |> 
    mutate(wei = 1/count) |> 
    select(-count)
}

# Tidy up datasets
reg_mod_data_tidy <- reg_mod_data_all %>%
  map(~rename(., baseline = scale)) |>
  modify_at("p2", ~ separate(.x, species, into = c("species","basin"), sep = "_")) |> 
  modify_at("p3", ~ separate(.x, species, into = c("species","basin","season"), sep = "_")) |> 
  modify_at("p4", ~ separate(.x, species, into = c("species","lake_region"), sep = "_")) |> 
  modify_at("p5", ~ separate(.x, species, into = c("species","lake_region","season"), sep = "_")) |> 
  modify_at(c(10:18), ~ separate(.x, species, into = c("ID","species"), sep = "_", extra = "merge")) |> 
  modify_at("p2_id", ~ separate(.x, species, into = c("species","basin"), sep = "_")) |> 
  modify_at("p3_id", ~ separate(.x, species, into = c("species","basin","season"), sep = "_")) |>
  modify_at("p4_id", ~ separate(.x, species, into = c("species","lake_region"), sep = "_")) |> 
  modify_at("p5_id", ~ separate(.x, species, into = c("species","lake_region","season"), sep = "_")) |> 
  # specific baselines need to add factor groups
  modify_at(c("s1","s2","s3","s4","s1_id","s2_id","s3_id","s4_id"), ~ mutate(.x, baseline1 = baseline)) |> 
  modify_at(c("s1","s1_id"), ~ rename(.x, basin = baseline)) |> 
  modify_at(c("s2","s2_id"), ~ rename(.x, lake_region = baseline)) |> 
  modify_at(c("s3","s3_id"), ~ rename(.x, basin = baseline)) |> 
  modify_at(c("s3","s3_id"), ~ separate(.x, basin, into = c("basin", "season"), sep = "_")) |> 
  modify_at(c("s4","s4_id"), ~ rename(.x, lake_region = baseline)) |> 
  modify_at(c("s4","s4_id"), ~ separate(.x, lake_region, into = c("lake_region", "season"), sep = "_")) |> 
  modify_at(c("s1","s2","s3","s4","s1_id","s2_id","s3_id","s4_id"), ~ rename(.x, baseline = baseline1)) |> 
  map(~relocate(., baseline, .after = species)) |>
  # Assign species weights to individual-level datasets
  modify_at(c(10:18), ~ assign_spp_weight(.x)) |>
  # Assign factors
  modify_if(~ any("basin" %in% colnames(.x)), ~ mutate(.x, basin = factor(basin))) |> 
  modify_if(~ any("lake_region" %in% colnames(.x)), ~ mutate(.x, lake_region = factor(lake_region))) |> 
  modify_if(~ any("season" %in% colnames(.x)), ~ mutate(.x, season = factor(season))) 

# Check output
str(reg_mod_data_tidy)

# Save cleaned datasets
save(reg_mod_data_tidy, file = here("out", "data", "reg_mod_data_v3.RData"))
load(file = here("out", "data", "reg_mod_data_v3.RData"))


# Viz data for models =======================================

tmp <- 4
plot(reg_mod_data_tidy[[tmp]]$Alpha_mode, reg_mod_data_tidy[[tmp]]$TP_mode)

reg_mod_data_tidy[[6]] |> 
  # ggplot(aes(mass_g, TP_mode)) +
  ggplot(aes(TP_mode, mass_g)) +
  geom_point(aes(color = Alpha_mode)) + 
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
  # scale_x_log10(breaks = c(1e-05,1e-04,1e-03,1e-02,1,150,1000)) 
  scale_y_log10(breaks = c(1e-05,1e-04,1e-03,1e-02,1,150,1000))

# reg_mod_data[[2]] |> 
#   ggplot(aes(TP_mode, mass_g)) +
#   geom_point(aes(color = Alpha_mode)) + 
#   ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
#   scale_y_log10(breaks = c(1e-06,1e-04, 1e-03, 1e-02, 1, 150, 1000), labels = scales::label_comma(accuracy = 1e-05))


xref <- df |> distinct(compartment, species)
reg_mod_data_tidy[[5]] |> 
  # separate(species, into = c("species", "region","season"), sep = "_") |>
  left_join(xref, by = "species") |> 
  ggplot(aes(species, TP_mode, color = scale)) +
  geom_point(size = 3, alpha = .5) + 
  theme(axis.text.x = element_text(angle=90))
  

reg_mod_data_tidy[[14]] |>
  # separate(species, into = c("species","region","season"), sep = "_") |> 
  # mutate(scale = paste(region, season, sep = "_")) |> 
  # ggplot(aes(TP_mode, mass_g)) +
  # ggplot(aes(mass_g, TP_mode)) +
  ggplot(aes(Alpha_mode, TP_mode)) +
  # ggplot(aes(Alpha_mode, mass_g)) +
  # geom_smooth(method = "lm", aes(color = group)) +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", color = "black") +
  # geom_point(size = 3, alpha = 0.5, aes(color = lake_region)) +
  geom_point(size = 3, alpha = 0.5, aes(color = baseline)) +
  # ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) + 
  scale_y_continuous(limits = c(1,5))
  # scale_x_log10(labels = scales::label_comma()) +
  # scale_y_log10(labels = scales::label_comma())
  # geom_point(size = 3, alpha = 0.5, aes(color = group))


reg_mod_data_tidy[[3]] |>
  mutate(group = if_else(Alpha_mode>=0.8, "phyto", "benthic")) |> 
  ggplot(aes(TP_mode, mass_g)) +
  geom_smooth(method = "lm", aes(color = group)) +
  scale_y_log10(breaks = c(1e-04, 1e-03, 1e-02, 1, 150, 1000), labels = scales::label_comma(accuracy = 1e-05))+ 
  geom_point(size = 3, alpha = 0.5, aes(color = group)) |

  reg_mod_data_tidy[[3]] |>
    mutate(group = if_else(Alpha_mode>=0.8, "phyto", "benthic")) |> 
    ggplot(aes(mass_g, TP_mode)) +
    geom_smooth(method = "lm", aes(color = group)) +
    scale_x_log10(breaks = c(1e-04, 1e-03, 1e-02, 1, 150, 1000), labels = scales::label_comma(accuracy = 1e-05))+  
    geom_point(size = 3, alpha = 0.5, aes(color = group))


