
# Use HBMs to assess associations between TP, alpha, and body size at different scales

# Prep ===============================
source(here::here("R", "00_prep.R"))

## Data - load either/or below
load(file = here("out", "data", "reg_mod_data_v3.RData"))
names(reg_mod_data_tidy)

# Controls ===================

n_cores = 4
n_chains = 4
n_thin = 1
adapt_d = .99
tree_depth = 20
n_iter = 500  # testing
# n_iter = 4000  # full


# MODEL Structures ============================================================

# Scale 1 - pooled baselines
model_str_1_pooled <- list(
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

model_str_1_ind <- list(
# Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1)),
  bf(TP_mode | weights(wei) ~ 1)
)

# # Scale 2a - specific, baselines by basin
# model_str_2a_basin <- list(
#   # Asymmetric TP-body size relationship
#   bf(TP_mode ~ log_mass * Alpha_mode + (1|basin)),
#   bf(TP_mode ~ log_mass + Alpha_mode + (1|basin)),
#   bf(TP_mode ~ log_mass * Alpha_mode + (log_mass|basin)),
#   bf(TP_mode ~ log_mass + Alpha_mode + (log_mass|basin)),
#   bf(TP_mode ~ 1 + (1|basin)),
#   # Coupling of different energy pathways - TP
#   bf(TP_mode ~ poly(Alpha_mode,2) + (1|basin)),
#   bf(TP_mode ~ poly(Alpha_mode,1) + (1|basin)),
#   bf(TP_mode ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|basin)),
#   bf(TP_mode ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|basin)),
#   bf(TP_mode ~ 1 + (1|basin)),
#   # Coupling of different energy pathways - Size
#   bf(log_mass ~ poly(Alpha_mode,2) + (1|basin)),
#   bf(log_mass ~ poly(Alpha_mode,1) + (1|basin)),
#   bf(log_mass ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|basin)),
#   bf(log_mass ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|basin)),
#   bf(log_mass ~ 1 + (1|basin))
# )

# Scale 2b - specific, baselines by lake_region
model_str_2b_region <- list(
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

# model_str_2a_ind <- list(
#   # Coupling of different energy pathways - TP
#   bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|basin)),
#   bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|basin)),
#   bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|basin)),
#   bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|basin)),
#   bf(TP_mode | weights(wei) ~ 1 + (1|basin))
# )

model_str_2b_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|lake_region)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|lake_region)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region)),
  bf(TP_mode | weights(wei) ~ 1 + (1|lake_region))
)

# Scale 3a - specific, baselines by basin x season
# model_str_3a_basin_season <- list(
#   # Asymmetric TP-body size relationship
#   bf(TP_mode ~ log_mass * Alpha_mode + (1|basin) + (1|season)),
#   bf(TP_mode ~ log_mass + Alpha_mode + (1|basin) + (1|season)),
#   bf(TP_mode ~ log_mass * Alpha_mode + (log_mass|basin) + (log_mass|season)),
#   bf(TP_mode ~ log_mass + Alpha_mode + (log_mass|basin) + (log_mass|season)),
#   bf(TP_mode ~ 1 + (1|basin) + (1|season)),
#   # Coupling of different energy pathways - TP
#   bf(TP_mode ~ poly(Alpha_mode,2) + (1|basin) + (1|season)),
#   bf(TP_mode ~ poly(Alpha_mode,1) + (1|basin) + (1|season)),
#   bf(TP_mode ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|basin) + (poly(Alpha_mode,2)|season)),
#   bf(TP_mode ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|basin) + (poly(Alpha_mode,1)|season)),
#   bf(TP_mode ~ 1 + (1|basin) + (1|season)),
#   # Coupling of different energy pathways - Size
#   bf(log_mass ~ poly(Alpha_mode,2) + (1|basin) + (1|season)),
#   bf(log_mass ~ poly(Alpha_mode,1) + (1|basin) + (1|season)),
#   bf(log_mass ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|basin) + (poly(Alpha_mode,2)|season)),
#   bf(log_mass ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|basin) + (poly(Alpha_mode,1)|season)),
#   bf(log_mass ~ 1 + (1|basin) + (1|season))
# )

# Scale 3b - specific, baselines by region x season
# Maybe add simulated littoral baselines for these two subsets????
model_str_3b_region_season <- list(
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

model_str_3a_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|basin) + (1|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|basin) + (1|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|basin) + (poly(Alpha_mode,2)|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|basin) + (poly(Alpha_mode,1)|season)),
  bf(TP_mode | weights(wei) ~ 1 + (1|basin) + (1|season))
)

model_str_3b_ind <- list(
  # Coupling of different energy pathways - TP
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,2) + (poly(Alpha_mode,2)|lake_region) + (poly(Alpha_mode,2)|season)),
  bf(TP_mode | weights(wei) ~ poly(Alpha_mode,1) + (poly(Alpha_mode,1)|lake_region) + (poly(Alpha_mode,1)|season)),
  bf(TP_mode | weights(wei) ~ 1 + (1|lake_region) + (1|season))
)



# MODEL Structures (asymmetric) ===================================

# Scale 1 - pooled baselines
model_str_1_pooled <- list(
  # Asymmetric TP-body size relationship
  bf(log_mass ~ TP_mode * Alpha_mode),
  bf(log_mass ~ TP_mode + Alpha_mode),
  bf(log_mass ~ 1)
)

# Scale 2a - specific, baselines by basin
model_str_2a_basin <- list(
  # Asymmetric TP-body size relationship
  bf(log_mass ~ TP_mode * Alpha_mode + (1|basin)),
  bf(log_mass ~ TP_mode + Alpha_mode + (1|basin)),
  bf(log_mass ~ TP_mode * Alpha_mode + (TP_mode|basin)),
  bf(log_mass ~ TP_mode + Alpha_mode + (TP_mode|basin)),
  bf(log_mass ~ 1 + (1|basin))
)

# Scale 2b - specific, baselines by lake_region
model_str_2b_region <- list(
  # Asymmetric TP-body size relationship
  bf(log_mass ~ TP_mode * Alpha_mode + (1|lake_region)),
  bf(log_mass ~ TP_mode + Alpha_mode + (1|lake_region)),
  bf(log_mass ~ TP_mode * Alpha_mode + (TP_mode|lake_region)),
  bf(log_mass ~ TP_mode + Alpha_mode + (TP_mode|lake_region)),
  bf(log_mass ~ 1 + (1|lake_region))
)


# Scale 3a - specific, baselines by basin x season
model_str_3a_basin_season <- list(
  # Asymmetric TP-body size relationship
  bf(log_mass ~ TP_mode * Alpha_mode + (1|basin) + (1|season)),
  bf(log_mass ~ TP_mode + Alpha_mode + (1|basin) + (1|season)),
  bf(log_mass ~ TP_mode * Alpha_mode + (TP_mode|basin) + (TP_mode|season)),
  bf(log_mass ~ TP_mode + Alpha_mode + (TP_mode|basin) + (TP_mode|season)),
  bf(log_mass ~ 1 + (1|basin) + (1|season))
)

# Scale 3b - specific, baselines by region x season
# Maybe add simulated littoral baselines for these two subsets????
model_str_3b_region_season <- list(
  # Asymmetric TP-body size relationship
  bf(log_mass ~ TP_mode * Alpha_mode + (1|lake_region) + (1|season)),
  bf(log_mass ~ TP_mode + Alpha_mode + (1|lake_region) + (1|season)),
  bf(log_mass ~ TP_mode * Alpha_mode + (TP_mode|lake_region) + (TP_mode|season)),
  bf(log_mass ~ TP_mode + Alpha_mode + (TP_mode|lake_region) + (TP_mode|season)),
  bf(log_mass ~ 1 + (1|lake_region) + (1|season))
)




# Fit models to data ===========================================================

## Species level ---------------------------------------------------------------

start_time <- Sys.time()

# Scale 1 - all pooled
brm_mods_1_asym <- model_str_1_pooled |> 
  map(
    ~brm(., reg_mod_data_tidy[["p1"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 2a - basin
brm_mods_2a_s1_asym <- model_str_2a_basin |> 
  map(
    ~brm(., reg_mod_data_tidy[["s1"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_2a_p2_asym <- model_str_2a_basin |> 
  map(
    ~brm(., reg_mod_data_tidy[["p2"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 2b - lake_region
brm_mods_2b_s2_asym <- model_str_2b_region |> 
  map(
    ~brm(., reg_mod_data_tidy[["s2"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_2b_p4_asym <- model_str_2b_region |> 
  map(
    ~brm(., reg_mod_data_tidy[["p4"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 3a - basin x season
brm_mods_3a_s3_asym <- model_str_3a_basin_season |> 
  map(
    ~brm(., reg_mod_data_tidy[["s3"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_3a_p3_asym <- model_str_3a_basin_season |> 
  map(
    ~brm(., reg_mod_data_tidy[["p3"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 3b - region x season
brm_mods_3b_s4_asym <- model_str_3b_region_season |> 
  map(
    ~brm(., reg_mod_data_tidy[["s4"]], 
      control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
      seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_3b_p5_asym <- model_str_3b_region_season |> 
  map(
    ~brm(., reg_mod_data_tidy[["p5"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

end_time <- Sys.time()
print(end_time - start_time)
# 1.4 hours

## Individual level ------------------------------------------------------------

start_time <- Sys.time()

# Scale 1
brm_mods_1_ind <- model_str_1_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p1_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 2a - basin
brm_mods_2a_s1_ind <- model_str_2a_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["s1_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_2a_p2_ind <- model_str_2a_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p2_id"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 2b - lake_region
brm_mods_2b_s2_ind <- model_str_2b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["s2_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_2b_p4_ind <- model_str_2b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p4_id"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 3a - basin x season
brm_mods_3a_s3_ind <- model_str_3a_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["s3_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_3a_p3_ind <- model_str_3a_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p3_id"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

# Scale 3b - region x season
brm_mods_3b_s4_ind <- model_str_3b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["s4_id"]],
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))

brm_mods_3b_p5_ind <- model_str_3b_ind |>
  map(
    ~brm(., reg_mod_data_tidy[["p5_id"]], 
         control = list(adapt_delta=adapt_d, max_treedepth = tree_depth),
         seed=12345, chains=n_chains, iter=n_iter, thin=n_thin, cores=n_cores))


end_time <- Sys.time()
print(end_time - start_time)


# Combine model lists into big list
brm_mods_list <- list(
  brm_mods_1, 
  brm_mods_2a_p2, 
  brm_mods_2a_s1, 
  brm_mods_2b_p4, 
  brm_mods_2b_s2, 
  brm_mods_3a_p3, 
  brm_mods_3a_s3, 
  brm_mods_3b_p5, 
  brm_mods_3b_s4, 
  brm_mods_1_ind, 
  brm_mods_2a_p2_ind, 
  brm_mods_2a_s1_ind, 
  brm_mods_2b_p4_ind, 
  brm_mods_2b_s2_ind, 
  brm_mods_3a_p3_ind, 
  brm_mods_3a_s3_ind, 
  brm_mods_3b_p5_ind,
  brm_mods_3b_s4_ind 
)

names(brm_mods_list) <- c(
  "brm_mods_1", 
  "brm_mods_2a_p2", 
  "brm_mods_2a_s1", 
  "brm_mods_2b_p4", 
  "brm_mods_2b_s2", 
  "brm_mods_3a_p3", 
  "brm_mods_3a_s3", 
  "brm_mods_3b_p5", 
  "brm_mods_3b_s4", 
  "brm_mods_1_ind", 
  "brm_mods_2a_p2_ind", 
  "brm_mods_2a_s1_ind", 
  "brm_mods_2b_p4_ind", 
  "brm_mods_2b_s2_ind", 
  "brm_mods_3a_p3_ind", 
  "brm_mods_3a_s3_ind", 
  "brm_mods_3b_p5_ind",
  "brm_mods_3b_s4_ind" 
)


# Save to file
save(brm_mods_list, file = here("out", "models", "brms", "brm_mods_list_v2.RData"))
load(file = here("out", "models", "brms", "brm_mods_list_v2.RData"))

brm_mods_list_asym <- list(
  brm_mods_1_asym, 
  brm_mods_2a_p2_asym, 
  brm_mods_2a_s1_asym, 
  brm_mods_2b_p4_asym, 
  brm_mods_2b_s2_asym, 
  brm_mods_3a_p3_asym, 
  brm_mods_3a_s3_asym, 
  brm_mods_3b_p5_asym, 
  brm_mods_3b_s4_asym
  )

names(brm_mods_list_asym) <- c(
  "brm_mods_1_asym", 
  "brm_mods_2a_p2_asym", 
  "brm_mods_2a_s1_asym", 
  "brm_mods_2b_p4_asym", 
  "brm_mods_2b_s2_asym", 
  "brm_mods_3a_p3_asym", 
  "brm_mods_3a_s3_asym", 
  "brm_mods_3b_p5_asym", 
  "brm_mods_3b_s4_asym"
)

save(brm_mods_list_asym, file = here("out", "models", "brms", "brm_mods_list_asym.RData"))

