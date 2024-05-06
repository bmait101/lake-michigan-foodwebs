
# Use HBMs to assess associations between TP, alpha, and body size at different scales

# Prep ===============================
# source(here::here("R", "00_prep.R"))
library(here)
library(tidyverse)
library(brms)

## Data - load either/or below
load(file = here("out", "data", "reg_mod_data_v3.RData"))
names(reg_mod_data_tidy)

# Controls ===================

n_cores = 4
n_chains = 4
n_thin = 1
adapt_d = .999
tree_depth = 20
# n_iter = 500  # testing
n_iter = 4000  # full


# Model Structures ============================================================

# Alpha_mode + I(Alpha_mode^2) + (I(Alpha_mode^2)|lake_region) + (I(Alpha_mode^2)|season))

# Scale 1 - pooled baselines
model_str_1_pooled <- list(
  bf(log_mass ~ TP_mode * Alpha_mode),
  bf(log_mass ~ TP_mode + Alpha_mode),
  bf(log_mass ~ 1),
  
  bf(TP_mode ~ Alpha_mode + I(Alpha_mode^2)),
  bf(TP_mode ~ Alpha_mode),
  bf(TP_mode ~ 1),
  
  bf(log_mass ~ Alpha_mode + I(Alpha_mode^2)),
  bf(log_mass ~ Alpha_mode),
  bf(log_mass ~ 1)
)

model_str_1_ind <- list(
  bf(TP_mode | weights(wei) ~ Alpha_mode + I(Alpha_mode^2)),
  bf(TP_mode | weights(wei) ~ Alpha_mode),
  bf(TP_mode | weights(wei) ~ 1)
)

# Scale 2b - specific, baselines by lake_region

model_str_2b_region <- list(
  bf(log_mass ~ TP_mode * Alpha_mode + (1|lake_region)),
  bf(log_mass ~ TP_mode + Alpha_mode + (1|lake_region)),
  bf(log_mass ~ TP_mode * Alpha_mode + (TP_mode|lake_region)),
  bf(log_mass ~ TP_mode + Alpha_mode + (TP_mode|lake_region)),
  bf(log_mass ~ 1 + (1|lake_region)),
  
  bf(TP_mode ~ Alpha_mode + I(Alpha_mode^2) + (1|lake_region)),
  bf(TP_mode ~ Alpha_mode + (1|lake_region)),
  bf(TP_mode ~ Alpha_mode + I(Alpha_mode^2) + (I(Alpha_mode^2)|lake_region)),
  bf(TP_mode ~ Alpha_mode + (Alpha_mode|lake_region)),
  bf(TP_mode ~ 1 + (1|lake_region)),
  
  bf(log_mass ~ Alpha_mode + I(Alpha_mode^2) + (1|lake_region)),
  bf(log_mass ~ Alpha_mode + (1|lake_region)),
  bf(log_mass ~ Alpha_mode + I(Alpha_mode^2) + (I(Alpha_mode^2)|lake_region)),
  bf(log_mass ~ Alpha_mode + (Alpha_mode|lake_region)),
  bf(log_mass ~ 1 + (1|lake_region))
)

model_str_2b_ind <- list(
  bf(TP_mode | weights(wei) ~ Alpha_mode + I(Alpha_mode^2) + (1|lake_region)),
  bf(TP_mode | weights(wei) ~ Alpha_mode + (1|lake_region)),
  bf(TP_mode | weights(wei) ~ Alpha_mode + I(Alpha_mode^2) + (I(Alpha_mode^2)|lake_region)),
  bf(TP_mode | weights(wei) ~ Alpha_mode + (Alpha_mode|lake_region)),
  bf(TP_mode | weights(wei) ~ 1 + (1|lake_region))
)

# Scale 3b - specific, baselines by region x season

model_str_3b_region_season <- list(
  bf(log_mass ~ TP_mode * Alpha_mode + (1|lake_region) + (1|season)),
  bf(log_mass ~ TP_mode + Alpha_mode + (1|lake_region) + (1|season)),
  bf(log_mass ~ TP_mode * Alpha_mode + (TP_mode|lake_region) + (TP_mode|season)),
  bf(log_mass ~ TP_mode + Alpha_mode + (TP_mode|lake_region) + (TP_mode|season)),
  bf(log_mass ~ 1 + (1|lake_region) + (1|season)),

  bf(TP_mode ~ Alpha_mode + I(Alpha_mode^2) + (1|lake_region) + (1|season)),
  bf(TP_mode ~ Alpha_mode + (1|lake_region) + (1|season)),
  bf(TP_mode ~ Alpha_mode + I(Alpha_mode^2) + (I(Alpha_mode^2)|lake_region) + (I(Alpha_mode^2)|season)),
  bf(TP_mode ~ Alpha_mode + (Alpha_mode|lake_region) + (Alpha_mode|season)),
  bf(TP_mode ~ 1 + (1|lake_region) + (1|season)),

  bf(log_mass ~ Alpha_mode + I(Alpha_mode^2) + (1|lake_region) + (1|season)),
  bf(log_mass ~ Alpha_mode + (1|lake_region) + (1|season)),
  bf(log_mass ~ Alpha_mode + I(Alpha_mode^2) + (I(Alpha_mode^2)|lake_region) + (I(Alpha_mode^2)|season)),
  bf(log_mass ~ Alpha_mode + (Alpha_mode|lake_region) + (Alpha_mode|season)),
  bf(log_mass ~ 1 + (1|lake_region) + (1|season))
)

model_str_3b_ind <- list(
  bf(TP_mode | weights(wei) ~ Alpha_mode + I(Alpha_mode^2) + (1|lake_region) + (1|season)),
  bf(TP_mode | weights(wei) ~ Alpha_mode + (1|lake_region) + (1|season)),
  bf(TP_mode | weights(wei) ~ Alpha_mode + I(Alpha_mode^2) + (I(Alpha_mode^2)|lake_region) + (I(Alpha_mode^2)|season)),
  bf(TP_mode | weights(wei) ~ Alpha_mode + (Alpha_mode|lake_region) + (Alpha_mode|season)),
  bf(TP_mode | weights(wei) ~ 1 + (1|lake_region) + (1|season))
)


# Fit models to data ===========================================================

start_time_all <- Sys.time()

# Scale 1 - all pooled
start_time <- Sys.time()
brm_mods_1 <- model_str_1_pooled |> 
  map(
    ~brm(., reg_mod_data_tidy[["p1"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

end_time <- Sys.time()
print(end_time - start_time)  # 9 mins
# save(brm_mods_1, file = here("out", "models", "brms", "mods_scl1.Rdata"))

start_time <- Sys.time()
brm_mods_1_ind <- model_str_1_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p1_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

end_time <- Sys.time()
print(end_time - start_time)  # 5 min
# save(brm_mods_1_ind, file = here("out", "models", "brms", "mods_scl1_ind.Rdata"))


# Scale 2b - lake_region

start_time <- Sys.time()
brm_mods_2b_s2 <- model_str_2b_region |> 
  map(
    ~brm(., reg_mod_data_tidy[["s2"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 20 mins - 4 and 1 divergent
# save(brm_mods_2b_s2, file = here("out", "models", "brms", "mods_scl2_s2.Rdata"))

start_time <- Sys.time()
brm_mods_2b_p4 <- model_str_2b_region |> 
  map(
    ~brm(., reg_mod_data_tidy[["p4"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 20 min - 1, 1, 1, 3, 1 divergent
# save(brm_mods_2b_p4, file = here("out", "models", "brms", "mods_scl2_p4.Rdata"))

start_time <- Sys.time()
brm_mods_2b_s2_ind <- model_str_2b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["s2_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 27 min - 2 divergent
# save(brm_mods_2b_s2_ind, file = here("out", "models", "brms", "mods_scl2_s2_ind.Rdata"))

start_time <- Sys.time()
brm_mods_2b_p4_ind <- model_str_2b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p4_id"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 26 min - 1 divergent
# save(brm_mods_2b_p4_ind, file = here("out", "models", "brms", "mods_scl2_p4_ind.Rdata"))


# Scale 3b - region x season

start_time <- Sys.time()
brm_mods_3b_s4 <- model_str_3b_region_season |> 
  map(
    ~brm(., reg_mod_data_tidy[["s4"]], 
      control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
      seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 39 min
# save(brm_mods_3b_s4, file = here("out", "models", "brms", "mods_scl3_s4.Rdata"))

start_time <- Sys.time()
brm_mods_3b_p5 <- model_str_3b_region_season |> 
  map(
    ~brm(., reg_mod_data_tidy[["p5"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 44 min
# save(brm_mods_3b_p5, file = here("out", "models", "brms", "mods_scl3_p5.Rdata"))

start_time <- Sys.time()
brm_mods_3b_s4_ind <- model_str_3b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["s4_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 29  min, 43 min with .999
# save(brm_mods_3b_s4_ind, file = here("out", "models", "brms", "mods_scl3_s4_ind.Rdata"))

start_time <- Sys.time()
brm_mods_3b_p5_ind <- model_str_3b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p5_id"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))
end_time <- Sys.time()
print(end_time - start_time)  # 30  min
# save(brm_mods_3b_p5_ind, file = here("out", "models", "brms", "mods_scl3_p5_ind.Rdata"))

end_time_all <- Sys.time()
print(end_time_all - start_time_all)


# 3.75 hours, 4.5 hour


## Save model files ============================================================

# Combine model lists into big list
brm_mods_list <- list(
  brm_mods_1, 
  brm_mods_2b_p4, 
  brm_mods_2b_s2, 
  brm_mods_3b_p5, 
  brm_mods_3b_s4, 
  brm_mods_1_ind, 
  brm_mods_2b_p4_ind, 
  brm_mods_2b_s2_ind, 
  brm_mods_3b_p5_ind,
  brm_mods_3b_s4_ind 
)

names(brm_mods_list) <- c(
  "brm_mods_1", 
  "brm_mods_2b_p4", 
  "brm_mods_2b_s2", 
  "brm_mods_3b_p5", 
  "brm_mods_3b_s4", 
  "brm_mods_1_ind", 
  "brm_mods_2b_p4_ind", 
  "brm_mods_2b_s2_ind", 
  "brm_mods_3b_p5_ind",
  "brm_mods_3b_s4_ind" 
)


# Save to file
save(brm_mods_list, file = here("out", "models", "brms", "brm_mods_list_20240328.RData"))
# load(file = here("out", "models", "brms", "brm_mods_list_v2.RData"))



# brm_mods_list_asym <- list(
#   brm_mods_1_asym, 
#   brm_mods_2a_p2_asym, 
#   brm_mods_2a_s1_asym, 
#   brm_mods_2b_p4_asym, 
#   brm_mods_2b_s2_asym, 
#   brm_mods_3a_p3_asym, 
#   brm_mods_3a_s3_asym, 
#   brm_mods_3b_p5_asym, 
#   brm_mods_3b_s4_asym
#   )
# 
# names(brm_mods_list_asym) <- c(
#   "brm_mods_1_asym", 
#   "brm_mods_2a_p2_asym", 
#   "brm_mods_2a_s1_asym", 
#   "brm_mods_2b_p4_asym", 
#   "brm_mods_2b_s2_asym", 
#   "brm_mods_3a_p3_asym", 
#   "brm_mods_3a_s3_asym", 
#   "brm_mods_3b_p5_asym", 
#   "brm_mods_3b_s4_asym"
# )
# 
# save(brm_mods_list_asym, file = here("out", "models", "brms", "brm_mods_list_asym.RData"))

