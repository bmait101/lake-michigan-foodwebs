
library(tidybayes)
library(broom.mixed)
library(patchwork)

# Prep
source(here::here("R", "00_prep.R"))

# Data
load(file = here("out", "data", "reg_mod_data_v3.RData"))

# Model objects
load(file = here("out", "models", "brms", "brm_mods_list_v2.RData"))

# List of models to get x_vert
m_list_verts <- list(
  brm_mods_list[["brm_mods_3b_s4"]][[11]],
  brm_mods_list[["brm_mods_3b_s4"]][[6]],
  brm_mods_list[["brm_mods_3b_s4_ind"]][[1]],
  brm_mods_list[["brm_mods_3b_p5"]][[11]],
  brm_mods_list[["brm_mods_3b_p5"]][[8]],
  brm_mods_list[["brm_mods_3b_p5_ind"]][[1]]
)


names(m_list_verts) <- c("Fig 1.b", "Fig 1.c", "Fig 1.d", "Fig 1.f", "Fig 1.g", "Fig 1.h")


## Approach 1 - random samples from posterior then get vertex =======

# Function to get 1000 random alpha values at vertex from posterior
get_x_vert_samp <- function(model) {
  model |> 
    spread_draws(`b_.*`, regex = TRUE) |> 
    slice_sample(n = 1000, replace = TRUE) |> 
    mutate(x_vert = (-b_Alpha_mode)/(2*b_IAlpha_modeE2))|> 
    select(x_vert)
}

vertex_sampled <- m_list_verts |> 
  map(get_x_vert_samp) |> 
  bind_cols() |> 
  pivot_longer(cols = everything(), names_to = "model") |> 
  mutate(model = case_when(
    model == "x_vert...1" ~ "Fig 3.b",
    model == "x_vert...2" ~ "Fig 3.c",
    model == "x_vert...3" ~ "Fig 3.d",
    model == "x_vert...4" ~ "Fig 3.f",
    model == "x_vert...5" ~ "Fig 3.g",
    model == "x_vert...6" ~ "Fig 3.h"
  ))

p.1 <- vertex_sampled |>
  ggplot(aes(y = value, x = model)) +
  geom_boxplot() + 
  geom_hline(yintercept = c(0,1), linetype = "dashed") +
  labs(y = "alpha at vertex", x = "")

p.2 <- vertex_sampled |>
  ggplot(aes(y = value, x = model)) +
  geom_boxplot(outliers = TRUE) + 
  # geom_hline(yintercept = c(0,1), linetype = "dashed") +
  labs(y = "alpha at vertex", x = "")

p.1 / p.2
