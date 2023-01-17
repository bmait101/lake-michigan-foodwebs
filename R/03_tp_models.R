# TP models

source(here::here("R", "00_prep.R"))


# Controls ===================

# for parallel processing
cl <- parallel::makePSOCKcluster(parallel::detectCores())


# Data ===============================

# Load prepped data subsets (ind is seperate b/c the run for hours)

load(file = here("out", "models", "tp", "tp_data_subs_2015.RData"))
load(file = here("out", "models", "tp", "tp_data_subs_2015_ind.RData"))


# Extract iso data =======================================================

iso_lists <- list()
for (i in 1:length(data_subsets_ind)){ # change object based on data subset object 
  iso_lists[[i]] <- extractIsotopeData(
    data_subsets_ind[[i]],  # change object based on data subset object 
    b1 = "b1", 
    b2 = "b2", 
    baselineColumn = "trophic", 
    consumersColumn = "species",
    groupsColumn = "scale",
    d13C = "d13C", 
    d15N = "d15N"
  )
}

# summary(iso_lists[[3]])
# plot(iso_lists[[3]]$`north_2-3072_bloater`)

# Run two baseline model =======================================================

# Out list (change object name depending on data)
# tp_mods_2015 <- list()
tp_mods_2015_ind <- list()

# Loop over datasets
for (i in 1:length(iso_lists)){ # change object based on data subset object 
  start_time <- Sys.time()
  tp_mods_2015_ind[[i]] <- parLapply(
    cl, 
    iso_lists[[i]],  # change object based on data subset object 
    multiModelTP,
    model = "twoBaselinesFull", lambda = 1,
    print = FALSE, 
    # n.chains = 2, n.iter = 100, burnin = 10, thin = 50 # testing
    n.chains = 5, n.iter = 100000, burnin = 10000, thin = 50
    )
  end_time <- Sys.time()
  print(end_time - start_time)
}

# Save
# save(tp_mods_2015, file = here("out", "models", "tp", "tp_mods_2015.RData"))
save(tp_mods_2015_ind, file = here("out", "models", "tp", "tp_mods_2015_ind.RData"))
