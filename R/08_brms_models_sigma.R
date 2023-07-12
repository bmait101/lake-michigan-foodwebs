
# Model sigma (variance structure) for models with 
# heterogeneous residuals (distributional models)
# "model the variability"

# Prep ===============================
source(here::here("R", "00_prep.R"))

# Controls ===================

ncores <- 5
iter <- 6000
adapt_delta <- 0.999

# Data ============

load(file = here("out", "data", "reg_mod_data_2015.RData"))

# Model structures ===============================================

# Scale 1
model_str_2015_01 <- list(
  # Asymmetric TP-body size relationship
  bf(TP_mode ~ log_mass * Alpha_mode, 
     sigma ~ log_mass * Alpha_mode),
  bf(TP_mode ~ log_mass + Alpha_mode, 
     sigma ~ log_mass + Alpha_mode),
  # Coupling of different energy pathways - TP
  bf(TP_mode ~ poly(Alpha_mode,2), 
     sigma ~ poly(Alpha_mode,2)),
  bf(TP_mode ~ poly(Alpha_mode,1), 
     sigma ~ poly(Alpha_mode,1)),
  # Coupling of different energy pathways - Size
  bf(log_mass ~ poly(Alpha_mode,2), 
     sigma ~ poly(Alpha_mode,2)),
  bf(log_mass ~ poly(Alpha_mode,1), 
     sigma ~ poly(Alpha_mode,1))
  )

model_str_2015_01_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2), 
     sigma ~ poly(Alpha_mode,2)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1), 
     sigma ~ poly(Alpha_mode,1))
  )


# Scale 2
model_str_2015_02 <- list(
  # Asymmetric TP-body size relationship
  bf(TP_mode ~ log_mass * Alpha_mode + (1|lake_region), 
     sigma ~ log_mass * Alpha_mode + (1|lake_region)),
  bf(TP_mode ~ log_mass + Alpha_mode + (1|lake_region), 
     sigma ~ log_mass + Alpha_mode + (1|lake_region)),
  # Coupling of different energy pathways - TP
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|lake_region), 
     sigma ~ poly(Alpha_mode,2) + (1|lake_region)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|lake_region),
     sigma ~ poly(Alpha_mode,1) + (1|lake_region)),
  # Coupling of different energy pathways - Size
  bf(log_mass ~ poly(Alpha_mode,2) + (1|lake_region), 
     sigma ~ poly(Alpha_mode,2) + (1|lake_region)),
  bf(log_mass ~ poly(Alpha_mode,1) + (1|lake_region), 
     sigma ~ poly(Alpha_mode,1) + (1|lake_region))
)

# Scale 2
model_str_2015_02_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|lake_region), 
     sigma ~ poly(Alpha_mode,2) + (1|lake_region)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|lake_region), 
     sigma ~ poly(Alpha_mode,1) + (1|lake_region))
)

# Scale 3
model_str_2015_03 <- list(
  # Asymmetric TP-body size relationship
  bf(TP_mode ~ log_mass * Alpha_mode + (1|lake_region) + (1|season), 
     sigma ~ log_mass * Alpha_mode + (1|lake_region) + (1|season)),
  bf(TP_mode ~ log_mass + Alpha_mode + (1|lake_region) + (1|season), 
     sigma ~ log_mass + Alpha_mode + (1|lake_region) + (1|season)),
  # Coupling of different energy pathways - TP
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season), 
     sigma ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season), 
     sigma ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season)),
  # Coupling of different energy pathways - Size
  bf(log_mass ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season), 
     sigma ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(log_mass ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season), 
     sigma ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season))
)

model_str_2015_03_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season), 
     sigma ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season), 
     sigma ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season))
)



# Fit models to data =========================================================

start_time <- Sys.time()

# 2015 - Scale 1
brm_mods_2015_01_sig <- 
  model_str_2015_01 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = adapt_delta, max_treedepth = 20),
      reg_mod_data[["scale01"]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 2a
brm_mods_2015_02a_sig <- 
  model_str_2015_02 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = adapt_delta, max_treedepth = 20),
      reg_mod_data[["scale02a"]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )


# 2015 - Scale 3a
brm_mods_2015_03a_sig <- 
  model_str_2015_03 |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = adapt_delta, max_treedepth = 20),
      reg_mod_data[["scale03a"]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 1 Ind
brm_mods_2015_01_ind_sig <- 
  model_str_2015_01_ind |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = adapt_delta, max_treedepth = 20),
      reg_mod_data[["scascale01_ind"]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 2 Ind
brm_mods_2015_02a_ind_sig <- 
  model_str_2015_02_ind |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = adapt_delta, max_treedepth = 20),
      reg_mod_data[["scascale02a_ind"]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )

# 2015 - Scale 3 Ind
brm_mods_2015_03a_ind_sig <- 
  model_str_2015_03_ind |> 
  map(
    ~brm(
      .,
      cores = ncores,
      control = list(adapt_delta = adapt_delta, max_treedepth = 20),
      reg_mod_data[["scascale03a_ind"]], 
      seed = 12345, chains = 4, iter = iter, thin = 1
    )
  )


end_time <- Sys.time()
print(end_time - start_time)

# 2.5 hours


# Combine model lists into big list
brm_mods_2015_sig <- list(
  brm_mods_2015_01_sig, 
  brm_mods_2015_02a_sig, 
  brm_mods_2015_03a_sig, 
  brm_mods_2015_01_ind_sig, 
  brm_mods_2015_02a_ind_sig, 
  brm_mods_2015_03a_ind_sig
)

names(brm_mods_2015_sig) <- c(
  "scale01", "scale02a", "scale03a", 
  "scascale01_ind", "scascale02a_ind", "scascale03a_ind"
)

# Save to file
save(brm_mods_2015_sig, file = here("out", "models", "brms", "brm_mods_2015_sig.RData"))
# load(file = here("out", "models", "brms", "brm_mods_2015.RData"))


