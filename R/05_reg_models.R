
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
model_structures_2015_01 <- list(
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

# Scale 2
model_structures_2015_02 <- list(
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

# Scale 3
model_structures_2015_03 <- list(
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


# Scale 1 - individual
# model_structures <- list(
#   # Coupling of different energy pathways - TP (ind level)
#   bf(TP_mode ~ poly(Alpha_mode,2)),
#   bf(TP_mode ~ poly(Alpha_mode,1)),
#   bf(TP_mode ~ 1)
# )



# Fit models to data =========================================================

# 2015 - Scale 1
start_time <- Sys.time()
brm_mods_2015_01 <- list()
for (i in 1:length(model_structures_2015_01)){
  brm_mods_2015_01[[i]] <- brm(
    model_structures_2015_01[[i]],
    cores = ncores,
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    df_mod_2015_01, 
    seed = 12345, 
    chains = 4, 
    iter = iter, 
    thin = 1
  )
}
end_time <- Sys.time()
print(end_time - start_time)
save(brm_mods_2015_01, file = here("out", "models", "brms", "2015_01.RData"))

# 2015 - Scale 2a
start_time <- Sys.time()
brm_mods_2015_02a <- list()
for (i in 1:length(model_structures_2015_02)){
  brm_mods_2015_02a[[i]] <- brm(
    model_structures_2015_02[[i]],
    cores = ncores,
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    df_mod_2015_02a, 
    seed = 12345, 
    chains = 4, 
    iter = iter, 
    thin = 1
  )
}
end_time <- Sys.time()
print(end_time - start_time)
save(brm_mods_2015_02a, file = here("out", "models", "brms", "2015_02a.RData"))

# 2015 - Scale 2b
start_time <- Sys.time()
brm_mods_2015_02b <- list()
for (i in 1:length(model_structures_2015_02)){
  brm_mods_2015_02b[[i]] <- brm(
    model_structures_2015_02[[i]],
    cores = ncores,
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    df_mod_2015_02a, 
    seed = 12345, 
    chains = 4, 
    iter = iter, 
    thin = 1
  )
}
end_time <- Sys.time()
print(end_time - start_time)
save(brm_mods_2015_02b, file = here("out", "models", "brms", "2015_02b.RData"))

# 2015 - Scale 3
start_time <- Sys.time()
brm_mods_2015_03a <- list()
for (i in 1:length(model_structures_2015_03)){
  brm_mods_2015_03a[[i]] <- brm(
    model_structures_2015_03[[i]],
    cores = ncores,
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    df_mod_2015_03a, 
    seed = 12345, 
    chains = 4, 
    iter = iter, 
    thin = 1
  )
}
end_time <- Sys.time()
print(end_time - start_time)
save(brm_mods_2015_03a, file = here("out", "models", "brms", "2015_03a.RData"))

# 2015 - Scale 3
start_time <- Sys.time()
brm_mods_2015_03b <- list()
for (i in 1:length(model_structures_2015_03)){
  brm_mods_2015_03b[[i]] <- brm(
    model_structures_2015_03[[i]],
    cores = ncores,
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    df_mod_2015_03b, 
    seed = 12345, 
    chains = 4, 
    iter = iter, 
    thin = 1
  )
}
end_time <- Sys.time()
print(end_time - start_time)
save(brm_mods_2015_03b, file = here("out", "models", "brms", "2015_03b.RData"))

