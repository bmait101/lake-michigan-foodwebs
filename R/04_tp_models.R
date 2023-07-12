# TP models



# Controls ===================

# for parallel processing
cl <- parallel::makePSOCKcluster(parallel::detectCores())


# Data ===============================

source(here::here("R", "00_prep.R"))
source(here::here("R", "02_prep_data.R"))


## Scales subsets ===================================================================

data_subs <- list(
  
  # Pooled baselines
  
  df |> mutate(scale = "pooled") |> droplevels() |> as.data.frame(),
  # df |> mutate(scale = "pooled", species = paste(species,basin,sep="_")) |> droplevels() |> as.data.frame(),
  df |> mutate(scale = "pooled", species = paste(species,lake_region,sep="_")) |> droplevels() |> as.data.frame(),
  df |> mutate(scale = "pooled", species = paste(species,lake_region,season,sep="_")) |> droplevels() |> as.data.frame(),
  # df |> mutate(scale = "pooled", species = paste(species,lake_region,season,year,sep="_")) |> droplevels() |> as.data.frame()

  # Specific baselines
  
  df |> mutate(scale = basin) |> droplevels() |> as.data.frame(),
  df |> mutate(scale = lake_region) |> droplevels() |> as.data.frame(),
  df |> mutate(scale = season) |> droplevels() |> as.data.frame(),
  df |> mutate(scale = paste(basin, season, sep = "_")) |> droplevels() |> as.data.frame(),
  df |> mutate(scale = paste(lake_region, season, sep = "_")) |> droplevels() |> as.data.frame()
  
  )

names(data_subs) <- c(
  "p1","p2","p3",
   "s1","s2","s3","s4","s5"
)

# Individual levels
# data_subs_15_ind <- list(
#   df |>
#     mutate(scale = "pooled") |>
#     mutate(species = paste(ID, species, sep = "_")) |>
#     droplevels() |> 
#     as.data.frame(),
#   df |>
#     mutate(scale = lake_region) |>
#     mutate(species = paste(ID, species, sep = "_")) |>
#     droplevels() |>
#     as.data.frame(),
#   df |>
#     mutate(scale = paste(lake_region, season, sep = "_")) |>
#     mutate(species = paste(ID, species, sep = "_")) |>
#     droplevels() |>
#     as.data.frame()
#   )
# 
# names(data_subs_15_ind) <- c(
#   "scale01_ind", "scale02_ind", "scale03_ind"
# )

save(data_subs, file = here("out", "data", "data_subs.RData"))
# save(data_subs_15_ind, file = here("out", "data", "data_subs_2015_ind_4regions.RData"))


# Viz data ----------------------------

data_subs$s5 |>
  # filter(! trophic %in% c("b1", "b2")) |>
  # mutate(trophic = case_when(
  #   species == "oligochaete" ~ "benthic",
  #   species == "amphipod" ~ "benthic",
  #   species == "chironomids" ~ "benthic",
  #   species == "dreissena" ~ "pelagic",
  #   TRUE ~ trophic
  # )) |>
  ggplot(aes(d13c_norm, d15n)) +
  geom_point(aes(color = trophic), size = 2, alpha = .5) +
  facet_wrap(vars(scale)) +
  labs(color = "Trophic Group", shape = "Trophic Group")

# plot_subsets <- function(datasub) {
#   p <- datasub |>
#     ggplot(aes(d13c_norm, d15n)) +
#     geom_point(aes(color = trophic), size = 2, alpha = .5) +
#     facet_wrap(vars(scale)) +
#     labs(color = "Trophic Group", shape = "Trophic Group")
#   p
# }
# 
# plots <- map(data_subs, plot_subsets)

# Extract iso data =======================================================

# By default, extractIsotopeData() uses Post's (2002) assumptions: 
# ∆N (n=56, 3.4 ± 0.98 sd) and ∆C (n= 107, 0.39 ± 1.3 sd)

# Or, use McCutchan's et al (2003) paper 
# ∆N (n=15, 2.9 ± 0.32 sd) and ∆C (n = 18, 1.3 ± 0.3 sd)
# TDF_values <- TDF(author = "McCutchan", element = "both", type = "muscle")

# change object based on data subset object 
iso_lists <- list()
for (i in 1:length(data_subs)){ # change object based on data subset object 
  iso_lists[[i]] <- extractIsotopeData(
    data_subs[[i]],  # change object based on data subset object 
    b1 = "b1", 
    b2 = "b2", 
    baselineColumn = "trophic", 
    consumersColumn = "species",
    groupsColumn = "scale",
    d13C = "d13c_norm", 
    d15N = "d15n"
    # deltaN = TDF_values$deltaN,
    # deltaC = TDF_values$deltaC
  )
}

summary(iso_lists[[8]])
# iso_lists[[1]]$`pooled-amphipod`
plot(iso_lists[[8]]$`se_fall-burbot`)

plot_names <- names(iso_lists[[1]])

for(i in 1:seq_along(plot_names)){
  plot(iso_lists[[4]]$i)
}


# Run two baseline model =======================================================

# Out list (change object name depending on data)
tp_mods <- list()
# tp_mods_2015_ind <- list()

# Loop over datasets
for (i in 1:length(iso_lists)){ # change object based on data subset object 
  start_time <- Sys.time()
  tp_mods[[i]] <- parLapply(
    cl, 
    iso_lists[[i]],  # change object based on data subset object 
    multiModelTP,
    model = "twoBaselinesFull", lambda = 1,
    print = FALSE, 
    n.chains = 2, n.iter = 10000, burnin = 5000, thin = 10 # testing
    # n.chains = 5, n.iter = 100000, burnin = 10000, thin = 50
    )
  end_time <- Sys.time()
  print(end_time - start_time)
}




# Save
save(tp_mods, file = here("out", "models", "tp", "tp_mods.RData"))
# save(tp_mods_2015_ind, file = here("out", "models", "tp", "tp_mods_2015_ind.RData"))

