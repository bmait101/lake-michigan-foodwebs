
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
tp_mods_2015_all <- c(tp_mods_2015, tp_mods_2015_ind)
data_subs_2015 <- c(data_subsets, data_subsets_ind)



# Extract TP data from Parallel ===========================

# Summarize TP data
tp_data_2015 <- tp_mods_2015_all |> map(summarize_TP_data)

str(tp_data_2015)
tp_data_2015[[6]]


# Data for HBMs models  ===================================================

# Map over data and tp data
df_reg_mod_data_2015 <- map2(data_subs_2015, tp_data_2015, avg_mass_and_merge)

# Give names to list datasets
names(df_reg_mod_data_2015) <- c(
  "scale01", "scale02a", "scale02b", "scale03a", "scale03b", 
  "scascale01_ind", "scascale02a_ind", "scascale03a_ind"
  )

# Check it
str(df_reg_mod_data_2015)


# Process the combined variables in each list element
df_reg_mod_data_2015 <- df_reg_mod_data_2015 %>%
  map(
    ~rename(., lake_region = scale)) |> 
  map(
    ~separate(., lake_region, into = c("lake_region", "season"), sep = "_")) |> 
  modify_if(function(y) any(is.na(y$season)), 
            ~select(.x, -season)) |> 
  modify_if(function(y) any(y$lake_region == "pooled"), 
            ~select(.x, -lake_region)) |> 
  modify_if(function(y) any(y$species == "alewife_north"), 
            ~separate(.x, species, into = c("species", "lake_region"), sep = "_")) |> 
  modify_if(function(y) any(y$species == "alewife_1"), 
            ~separate(.x, species, into = c("species", "season"), sep = "_")) 

# Convert factors
df_reg_mod_data_2015[[2]]$lake_region <- factor(df_reg_mod_data_2015[[2]]$lake_region)
df_reg_mod_data_2015[[3]]$lake_region <- factor(df_reg_mod_data_2015[[3]]$lake_region)
df_reg_mod_data_2015[[4]]$lake_region <- factor(df_reg_mod_data_2015[[4]]$lake_region)
df_reg_mod_data_2015[[4]]$season <- factor(df_reg_mod_data_2015[[4]]$season)
df_reg_mod_data_2015[[5]]$lake_region <- factor(df_reg_mod_data_2015[[5]]$lake_region)
df_reg_mod_data_2015[[5]]$season <- factor(df_reg_mod_data_2015[[5]]$season)
df_reg_mod_data_2015[[7]]$lake_region <- factor(df_reg_mod_data_2015[[7]]$lake_region)
df_reg_mod_data_2015[[8]]$lake_region <- factor(df_reg_mod_data_2015[[8]]$lake_region)
df_reg_mod_data_2015[[8]]$season <- factor(df_reg_mod_data_2015[[8]]$season)

str(df_reg_mod_data_2015)

save(df_reg_mod_data_2015, file = here("out", "data", "reg_mod_data_2015.RData"))


# Viz data for models =======================================

df_reg_mod_data_2015[[1]] |>
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




