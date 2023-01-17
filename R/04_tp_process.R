
# Process TP models

# Prep ===============================
source(here::here("R", "00_prep.R"))

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

# Function: summarize body size, merge TP data, and log transform body mass
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


# Data ===============

# Data subsets used for TP models
load(file = here("out", "models", "tp", "tp_data_subs_2015.RData"))
load(file = here("out", "models", "tp", "tp_data_subs_2015_ind.RData"))

# TP model objects
load(file = here("out", "models", "tp", "tp_mods_2015.RData"))
load(file = here("out", "models", "tp", "tp_mods_2015_ind.RData"))

# Combine individual estimates with species levels
tp_mods <- c(tp_mods_2015, tp_mods_2015_ind)
data_subs <- c(data_subsets, data_subsets_ind)



# Extract TP data from Parallel ===========================

# Summarize TP data
tp_data_2015 <- tp_mods |> map(summarize_TP_data)

str(tp_data_2015)
tp_data_2015[[6]]


# Data for HBMs models  ===================================================

# Map over data and tp data
df_mods_2015 <- map2(data_subs, tp_data_2015, avg_mass_and_merge)

# Give names to list datasets
names(df_mods_2015) <- c(
  "scale01", "scale02a", "scale02b", "scale03a", "scale03b", 
  "scascale01_ind", "scascale02a_ind", "scascale03a_ind"
  )

# Check it
str(df_mods_2015)


# Need to process the scale variable in each list element
df_mods_2015 %>%
  map(~mutate(., NewColumn1 = first/(second*2), NewColumn2 = first/(second*3)))


# Viz data for models =======================================

df_mods_2015[[1]] |>
  # filter(!(species %in% c("deepwater sculpin", "oligochaete", "round goby", "lake whitefish"))) |>
  ggplot(aes(Alpha_mode, TP_mode)) +
  # ggplot(aes(Alpha_mode, mass_g)) +
  # ggplot(aes(mass_g, TP_mode)) +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", color = "black") +
  geom_point(size = 3, alpha = 0.5) +
  # geom_point(size = 3, alpha = 0.5, aes(color = lake_region)) +
  # geom_point(size = 3, alpha = 0.5, aes(color = season, shape = lake_region)) +
  # scale_x_log10(labels = scales::label_comma()) +
  # scale_y_log10(labels = scales::label_comma()) +
  # ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
  theme_clean()




