
# Use HBMs to assess associattions between TP, alpha, and body size at different sclaes

# Prep ===============================
source(here::here("R", "00_prep.R"))

# Controls ===================

ncores <- 5
iter <- 2000


# Data ============

load(file = here("out", "data", "reg_mod_data_2015.RData"))

# MODEL Structures ============================================================

# Scale 1
model_str_2015_01 <- list(
  # Asymmetric TP-body size relationship
  bf(TP_mode ~ log_mass * Alpha_mode),
  bf(TP_mode ~ log_mass + Alpha_mode),
  bf(TP_mode ~ 1),
  # Coupling of different energy pathways - TP
  bf(TP_mode ~ poly(Alpha_mode,2)),
  bf(TP_mode ~ poly(Alpha_mode,1)),
  bf(TP_mode ~ 1),
  # Coupling of different energy pathways - Size
  bf(log_mass ~ poly(Alpha_mode,2)),
  bf(log_mass ~ poly(Alpha_mode,1)),
  bf(log_mass ~ 1)
)

model_str_2015_01_ind <- list(
# Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1)),
  bf(TP_mode | weights(wei) ~ 1)
)

# Scale 2
model_str_2015_02 <- list(
  # Asymmetric TP-body size relationship
  bf(TP_mode ~ log_mass * Alpha_mode + (1|lake_region)),
  bf(TP_mode ~ log_mass + Alpha_mode + (1|lake_region)),
  bf(TP_mode ~ log_mass * Alpha_mode + (log_mass|lake_region)),
  bf(TP_mode ~ log_mass + Alpha_mode + (log_mass|lake_region)),
  bf(TP_mode ~ 1 + (1|lake_region)),
  # Coupling of different energy pathways - TP
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|lake_region)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|lake_region)),
  bf(TP_mode ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region)),
  bf(TP_mode ~ 1 + (1|lake_region)),
  # Coupling of different energy pathways - Size
  bf(log_mass ~ poly(Alpha_mode,2) + (1|lake_region)),
  bf(log_mass ~ poly(Alpha_mode,1) + (1|lake_region)),
  bf(log_mass ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region)),
  bf(log_mass ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region)),
  bf(log_mass ~ 1 + (1|lake_region))
)

# Scale 2
model_str_2015_02_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|lake_region)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|lake_region)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region)),
  bf(TP_mode | weights(wei) ~ 1 + (1|lake_region))
)

# Scale 3
model_str_2015_03 <- list(
  # Asymmetric TP-body size relationship
  bf(TP_mode ~ log_mass * Alpha_mode + (1|lake_region) + (1|season)),
  bf(TP_mode ~ log_mass + Alpha_mode + (1|lake_region) + (1|season)),
  bf(TP_mode ~ log_mass * Alpha_mode + (log_mass|lake_region) + (log_mass|season)),
  bf(TP_mode ~ log_mass + Alpha_mode + (log_mass|lake_region) + (log_mass|season)),
  bf(TP_mode ~ 1 + (1|lake_region) + (1|season)),
  # Coupling of different energy pathways - TP
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season)),
  bf(TP_mode ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region) + (poly(Alpha_mode,2)|season)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region) + (poly(Alpha_mode,1)|season)),
  bf(TP_mode ~ 1 + (1|lake_region) + (1|season)),
  # Coupling of different energy pathways - Size
  bf(log_mass ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(log_mass ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season)),
  bf(log_mass ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region) + (poly(Alpha_mode,2)|season)),
  bf(log_mass ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region) + (poly(Alpha_mode,1)|season)),
  bf(log_mass ~ 1 + (1|lake_region) + (1|season))
)

model_str_2015_03_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region) + (poly(Alpha_mode,2)|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region) + (poly(Alpha_mode,1)|season)),
  bf(TP_mode | weights(wei) ~ 1 + (1|lake_region) + (1|season))
)



# Fit models to data =========================================================

start_time <- Sys.time()

# 2015 - Scale 1
brm_mods_2015_01 <- 
  model_str_2015_01 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[1]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 1 Ind
brm_mods_2015_01_ind <- 
  model_str_2015_01_ind |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[6]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 2a
brm_mods_2015_02a <- 
  model_str_2015_02 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[2]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 2 Ind
brm_mods_2015_02a_ind <- 
  model_str_2015_02_ind |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[7]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 2b
brm_mods_2015_02b <- 
  model_str_2015_02 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[3]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 3a
brm_mods_2015_03a <- 
  model_str_2015_03 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[4]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 3 Ind
brm_mods_2015_03a_ind <- 
  model_str_2015_03_ind |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[8]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 3b
brm_mods_2015_03b <- 
  model_str_2015_03 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      reg_mod_data_2015[[5]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

end_time <- Sys.time()
print(end_time - start_time)

brm_mods_2015 <- list(
  brm_mods_2015_01, 
  brm_mods_2015_01_ind, 
  brm_mods_2015_02a, 
  brm_mods_2015_02a_ind, 
  brm_mods_2015_02b, 
  brm_mods_2015_03a, 
  brm_mods_2015_03a_ind, 
  brm_mods_2015_03b
)

names(brm_mods_2015) <- c(
  "scale01", "scale01_ind",
  "scale02a", "scale02a_ind", "scale02b", 
  "scale03a", "scale03a_ind", "scale03b"
)


save(brm_mods_2015, file = here("out", "models", "brms", "brm_mods_2015.RData"))
load(file = here("out", "models", "brms", "brm_mods_2015.RData"))
