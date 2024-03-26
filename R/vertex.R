library(broom.mixed)

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

# Approach 1 - get vertex and CIs using coefs and std errors ========

# Test model
m <- m_list_verts[[1]]

m |> 
  spread_draws(`b_.*`, regex = TRUE) |> 
  summarise_draws()

# Get the coefficients
m_coefs <- m |> 
  tidy() |> 
  filter(term %in% c("polyAlpha_mode21", "polyAlpha_mode22")) |> 
  select(term, estimate, std.error, conf.low, conf.high)
m_coefs

a <- m_coefs |> filter(term == "polyAlpha_mode21") |> pull(estimate)|> unname()
b <- m_coefs |> filter(term == "polyAlpha_mode22") |> pull(estimate) |> unname()

if(2*b>0){ #Check second deriv.
  -a/(2*b)
} else{ # concave down
  NA
}

-a/(2*b)

a <- m_coefs |> filter(term == "polyAlpha_mode21") |> pull(estimate)|> unname()
b <- m_coefs |> filter(term == "polyAlpha_mode22") |> pull(estimate) |> unname()


# Find lower CI vertex
a <- m_coefs |> filter(term == "polyAlpha_mode21") |> pull(conf.low)|> unname()
b <- m_coefs |> filter(term == "polyAlpha_mode22") |> pull(conf.low) |> unname()
-a/(2*b)

# Find upper CI vertex
a <- m_coefs |> filter(term == "polyAlpha_mode21") |> pull(conf.high)|> unname()
b <- m_coefs |> filter(term == "polyAlpha_mode22") |> pull(conf.high) |> unname()
-a/(2*b)


# Approach 2 - get vertex using 89% HDI ========

c_coefs_hdi <- m |>
  spread_draws(`b_.*`, regex = TRUE) |>
  mean_hdi(.width = .89)

-c_coefs_hdi$`b_polyAlpha_mode21`/(2*c_coefs_hdi$`b_polyAlpha_mode22`)
-c_coefs_hdi$b_polyAlpha_mode21.lower/(2*c_coefs_hdi$b_polyAlpha_mode22.lower)
-c_coefs_hdi$b_polyAlpha_mode21.upper/(2*c_coefs_hdi$b_polyAlpha_mode22.upper)

get_vertex_hdi <- function(model) {
  # extract coefficients and HDI
  c_coefs_hdi <- model |> 
    spread_draws(`b_.*`, regex = TRUE) |>
    mean_hdi(.width = .89)
  
  # calculate vertex
  vert_mean <- -c_coefs_hdi$`b_polyAlpha_mode21`/(2*c_coefs_hdi$`b_polyAlpha_mode22`)
  vert_lower <- -c_coefs_hdi$b_polyAlpha_mode21.lower/(2*c_coefs_hdi$b_polyAlpha_mode22.lower)
  vert_upper <- -c_coefs_hdi$b_polyAlpha_mode21.upper/(2*c_coefs_hdi$b_polyAlpha_mode22.upper)
  
  # return a data frame with values
  data.frame(vert_mean = vert_mean, vert_lower = vert_lower, vert_upper = vert_upper)
  }

get_vertex_hdi(m)

vertex_hdi <- 
  map(m_list_verts, get_vertex_hdi) |> 
  bind_rows() |> 
  mutate(model = c("Fig 3.b", "Fig 3.c", "Fig 3.d", "Fig 3.f", "Fig 3.g", "Fig 3.h")) |> 
  relocate(model, .before = vert_mean)
vertex_hdi

## Approach 3 - get vertex for every posterior sample =========

m |> 
  spread_draws(`b_.*`, regex = TRUE) |> 
  mutate(x_vert = (-b_polyAlpha_mode21)/(2*b_polyAlpha_mode22)) |> 
  select(x_vert) |> 
  ggplot(aes(x = x_vert)) +
  geom_boxplot(outliers = FALSE)

# Function to get x vertex
get_x_vert <- function(model) {
  model |> 
    spread_draws(`b_.*`, regex = TRUE) |> 
    mutate(x_vert = (-b_polyAlpha_mode21)/(2*b_polyAlpha_mode22)) |> 
    select(x_vert)
}

# Get x_vert for each model
vert <- map(m_list_verts, get_x_vert)

# Combine into a single data frame and plot
vert |> 
  bind_cols() |> 
  pivot_longer(cols = everything(), names_to = "model") |> 
  ggplot(aes(y = value, x = model)) + 
  geom_boxplot(outliers = FALSE)


## Approach 4 - random samples from posterior then get vertex =======

m |> 
  spread_draws(`b_.*`, regex = TRUE) |> 
  slice_sample(n = 1000, replace = TRUE) |> 
  mutate(x_vert = (-b_polyAlpha_mode21)/(2*b_polyAlpha_mode22))|> 
  select(x_vert)

# Function to get x vertex
get_x_vert_samp <- function(model) {
  model |> 
    spread_draws(`b_.*`, regex = TRUE) |> 
    slice_sample(n = 1000, replace = TRUE) |> 
    mutate(x_vert = (-b_polyAlpha_mode21)/(2*b_polyAlpha_mode22))|> 
    select(x_vert)
}

vertex_sampled <- m_list_verts |> map(get_x_vert_samp) 

# vertex_sampled |> 
#   bind_cols() |>
#   ggplot(aes(y = value)) +
#   geom_boxplot(outliers = FALSE)

vertex_sampled <- vertex_sampled |> 
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
  geom_boxplot(outliers = FALSE) + 
  geom_hline(yintercept = c(0,1), linetype = "dashed") +
  labs(y = "alpha at vertex", x = "")

p.2 <- vertex_sampled |>
  ggplot(aes(y = value, x = model)) +
  geom_boxplot(outliers = TRUE) + 
  # geom_hline(yintercept = c(0,1), linetype = "dashed") +
  labs(y = "alpha at vertex", x = "")

p.1 / p.2

ggsave(
  here("out","plots","R1","vertex_sampled.png"),
  last_plot(), 
  height = 12, 
  width = 16, 
  units = "cm", 
  device = png
)


