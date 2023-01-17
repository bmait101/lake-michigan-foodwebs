
# 1) calculate trophic position and alpha values for each species at different 
# scales using the Two Baseline Full model approach; 
# 2) Run hierarchical bayesian models with brms to explore the association 
# between alpha and TP; 
# 3) Compare models using different metrics (R2, LOO); and 
# 4) Create some diagnostic plots.

## Prep ============

# for parallel processing
cl <- parallel::makePSOCKcluster(parallel::detectCores())
ncores <- 5

## Data ============

df <- data |> 
  select(
    dataset, lake_region, year, season, depth_m, 
    compartment, species, species_group, 
    d13C = d13c_norm,  d15N = d15n, length_mm, mass_g
    ) |> 
  rowid_to_column("ID") |> 
  drop_na(d13C, d15N, lake_region, year, season) 

df_01 <- df |> 
  filter(dataset %in% c("csmi_2015", "nps_2015")) |>
  # filter(dataset %in% c("kornis_2014", "csmi_2015", "nps_2015", "glft_2016")) |>
  # filter(dataset %in% c("csmi_2015","nps_2015","glft_2016","roth_2019")) |>
  filter(!compartment %in% c("ichthoplankton")) |>
  # filter(!compartment %in% c("ichthoplankton", "pom", "macro-alga")) |>
  # filter(!species %in% c("slimy sculpin", "deepwater sculpin")) |>
  # mutate(lake_region = "pooled") |>
  mutate(lake_region = case_when(
    lake_region %in% c("nw", "ne") ~ "north",
    lake_region %in% c("sw", "se") ~ "south"
  )) |>
  mutate(lake_region = paste(lake_region, season, sep = "_")) |>
  # mutate(species = paste(species, year, lake_region, season, sep = "_")) |>
  # mutate(species = paste(ID, species, sep = "_")) |>
  # filter(! lake_region %in% c("ne_1", "se_1")) |>
  mutate(trophic = case_when(
    compartment == "pom" ~ "b1", compartment == "macro-alga" ~ "b2", TRUE ~ "consumer"
  )) |>
  # mutate(trophic = case_when(
  #   compartment == "dreissenids" ~ "b1",
  #   species_group %in% c("amphipod")  ~ "b2",
  #   TRUE ~ "consumer"
  # )) |>
  droplevels() |> 
  as.data.frame()

# Viz data
df_01 |> 
  ggplot(aes(d13C, d15N)) + 
  geom_point(aes(color = trophic), size = 2, alpha = .5) + 
  facet_wrap(vars(lake_region)) +
  labs(color = "Trophic Group", shape = "Trophic Group")


# screenFoodWeb(df, grouping = c("species", "trophic"))
# summariseIsotopeData(df, grouping = c("species", "trophic"))
# ggsave(here("out", "plots", "TPdata_2015_pooled.png"),
#        width = 12, height = 8, dpi = 300)


## TP and alpha ===============

IsotopesList <- extractIsotopeData(
  df_01,
  b1 = "b1",
  b2 = "b2", 
  baselineColumn = "trophic", 
  consumersColumn = "species",
  groupsColumn = "lake_region",
  d13C = "d13C", 
  d15N = "d15N"
)

summary(IsotopesList)
plot(IsotopesList$`south-lake trout`)

start_time <- Sys.time()

# Run two baseline model (Increase n.iter, burn in, thin 4 model convergence)
TP_model <- parLapply(
  cl, IsotopesList, multiModelTP,
  model = "twoBaselinesFull", lambda = 1,
  # print = TRUE, n.chains = 5, n.iter = 1000, burnin = 100, thin = 1
  print = TRUE, n.chains = 5, n.iter = 100000, burnin = 10000, thin = 50
) 

# save (TP_model, file = here("out", "models", "tp", "TP_model_corr-fish.RData"))
# load ("TP_model.RData")

# Summarize TP data
TP_data <- fromParallelTP(TP_model, get = "summary")
head(TP_data)
colnames(TP_data) <- c(
  'model','group',"species",
  'TP_lower','TP_upper','TP_median','TP_mode',
  'Alpha_lower','Alpha_upper','Alpha_median','Alpha_mode'
)

TP_data <- TP_data |>
  # separate(species, into = c("species","year","lake_region","season"), sep = "_") |> 
  separate(group, into = c("lake_region", "season"), sep = "_")
  

# Data for models: average body size, merge TP data, and log transform body mass
df_mod <- df_01 %>%
  filter(trophic == "consumer") %>%
  # select(species, mass_g) %>%
  select(species, lake_region, mass_g) %>%
  separate(lake_region, into = c("lake_region", "season"), sep = "_") %>%
  # separate(species, into = c("species","year","lake_region", "season"), sep = "_") %>%
  # group_by(species) %>%
  group_by(species, lake_region, season) %>%
  summarise(mass_g = mean(mass_g, na.rm = TRUE), .groups = 'drop') %>%
  # left_join(TP_data, by=c("species")) %>%
  left_join(TP_data, by=c("species", "lake_region", "season")) %>%
  mutate(
    # lake_region = factor(lake_region),
    # year = factor(year),
    # season = factor(season),
    log_mass = log(mass_g)
  ) %>%
  drop_na(mass_g) |> 
  as.data.frame()


# Plot data
df_mod |> 
  ggplot(aes(Alpha_mode, TP_mode)) +
  # ggplot(aes(Alpha_mode, mass_g)) +
  # ggplot(aes(mass_g, TP_mode)) +
  # geom_point(size = 3, alpha = 0.5, shape = 21) +
  # geom_smooth(method = "lm", color = "black") +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", color = "black") +
  # geom_point(size = 3, alpha = 0.5) +
  geom_point(aes(color = season, shape = lake_region), size = 3, alpha = 0.5) +
  # scale_x_log10(labels = scales::label_comma()) +
  # scale_y_log10(labels = scales::label_comma()) +
  # ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
  # labs(title = "2015-2019, region baseline, by season",x = "Alpha", y = "TP") +
  # xlim(c(0,1)) +  
  # ylim(c(0.5,5)) + 
  theme_clean()

# ggsave(here("out", "plots", "01_b_label.png"),
#        width = 10, height = 8, dpi = 300)



# RUN MODELS ===================================================================

# Different fixed structure + random intercept component

# model_structures <- list(
#   bf(TP_mode ~ poly(Alpha_mode,2)),  # Quadratic
#   bf(TP_mode ~ poly(Alpha_mode,1)),  # Linear
#   bf(TP_mode ~ 1)                   # Null
# )
# 
# model_structures <- list(
#   bf(TP_mode ~ poly(Alpha_mode,2) + (1|grp)),  # Quadratic
#   bf(TP_mode ~ poly(Alpha_mode,1) + (1|grp)),  # Linear
#   bf(TP_mode ~ 1 + (1|grp))                   # Null
# )

# Pooled data
# model_structures <- list(
#   bf(TP_mode ~ log_mass * Alpha_mode),
#   bf(TP_mode ~ log_mass + Alpha_mode),
#   bf(TP_mode ~ 1),
#   
#   bf(TP_mode ~ poly(Alpha_mode,2)),
#   bf(TP_mode ~ poly(Alpha_mode,1)),
#   bf(TP_mode ~ 1),
#   
#   bf(log_mass ~ poly(Alpha_mode,2)),
#   bf(log_mass ~ poly(Alpha_mode,1)),
#   bf(log_mass ~ 1)
# )

# Random effects
# model_structures <- list(
#   bf(TP_mode ~ log_mass * Alpha_mode + (1|lake_region) + (1|year/season)),
#   bf(TP_mode ~ log_mass + Alpha_mode + (1|lake_region)+ (1|year/season)),
#   bf(TP_mode ~ 1 + (1|lake_region)+ (1|year/season)),
# 
#   bf(TP_mode ~ poly(Alpha_mode,2) + (1|lake_region)+ (1|year/season)),
#   bf(TP_mode ~ poly(Alpha_mode,1) + (1|lake_region)+ (1|year/season)),
#   bf(TP_mode ~ 1 + (1|lake_region)),
# 
#   bf(log_mass ~ poly(Alpha_mode,2) + (1|lake_region)+ (1|year/season)),
#   bf(log_mass ~ poly(Alpha_mode,1) + (1|lake_region)+ (1|year/season)),
#   bf(log_mass ~ 1 + (1|lake_region)+ (1|year/season))
# )

model_structures <- list(
  bf(TP_mode ~ log_mass * Alpha_mode + (1|lake_region) + (1|season)),
  bf(TP_mode ~ log_mass + Alpha_mode + (1|lake_region)),
  bf(TP_mode ~ 1 + (1|lake_region)),

  bf(TP_mode ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season)),
  bf(TP_mode ~ 1 + (1|lake_region) + (1|season)),

  bf(log_mass ~ poly(Alpha_mode,2) + (1|lake_region) + (1|season)),
  bf(log_mass ~ poly(Alpha_mode,1) + (1|lake_region) + (1|season)),
  bf(log_mass ~ 1 + (1|lake_region) + (1|season))
)

# Fit models to data
# names(model_structures) <- c("Quadratic","Linear", "Null")

models <- list()
for (i in 1:length (model_structures)){
  models[[i]] <- brm(
    model_structures[[i]],
    cores = ncores,
    control = list(adapt_delta = 0.99, max_treedepth = 10),
    df_mod, seed = 12345, iter = 2000, thin = 1
  )
}

end_time <- Sys.time()

end_time - start_time
# save models
# save(models, file="models_TP_alpha.R")
# load("models_TP_alpha.R")



# Model comparison =================
# compare the different models using different metrics elpd, looic, r2

# models_sub <- models[1:2]
# comp <- loo_compare(
#   loo(models_sub[[1]]),
#   loo(models_sub[[2]])
# )

models_sub <- models[1:3]
models_sub <- models[4:6]
models_sub <- models[7:9]

comp <- loo_compare(
  loo(models_sub[[1]]),
  loo(models_sub[[2]]),
  loo(models_sub[[3]])
)

comp_summary <-
  print(comp, simplify = FALSE, digits = 3) %>%
  as.data.frame()

model_sel_tab <- data.frame (elpd_diff = rep(NA, nrow(comp_summary)),
                             elpd = rep(NA, nrow(comp_summary)),
                             p_loo = rep(NA, nrow(comp_summary)),
                             looic = rep(NA, nrow(comp_summary)),
                             r2_loo = rep(NA, nrow(comp_summary)),
                             r2_marg = rep(NA, nrow(comp_summary)),
                             r2_cond = rep(NA, nrow(comp_summary)),
                             row.names= rownames(comp_summary))


model_sel_tab$elpd_diff <- paste (comp_summary$elpd_diff %>% round(digits=2), " (",
                                  comp_summary$se_diff %>% round(digits=2), ")",sep="")
model_sel_tab$elpd <- paste (comp_summary$elpd_loo %>% round(digits=2), " (",
                             comp_summary$se_elpd_loo %>% round(digits=2), ")",sep="")

model_sel_tab$p_loo <- paste (comp_summary$p_loo %>% round(digits=2)," (",
                              comp_summary$se_p_loo %>% round(digits=2), ")",sep="")

model_sel_tab$looic <- paste (comp_summary$looic %>% round(digits=2)," (",
                              comp_summary$se_looic %>% round(digits=2), ")",sep="")

# count <- 1
for (i in 1:length(models_sub)){
  r2_marg_cond <- r2_bayes(get("models_sub")[[i]])
  model_sel_tab[i,"r2_loo"] <- loo_R2 (get("models_sub")[[i]]) %>% round(digits=2)
  model_sel_tab[i,"r2_marg"] <- paste (r2_marg_cond$R2_Bayes_marginal %>% round(digits=2)," (",
                                       attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),
                                       ")",sep="")
  model_sel_tab[i,"r2_cond"] <- paste (r2_marg_cond$R2_Bayes %>% round(digits=2)," (",
                                       attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),
                                       ")",sep="")
  # count <- count + 1
}

model_sel_tab
# save(model_sel_tab, file = here("out", "tbls", "2015-19_tbl_model_sel.R"))
# load("models_TP_alpha.R")



# Explore the relationships =================

# select best model number (1, 2, or 3) for summaries and diagnostics
best_model_num <- 1
best_model_num <- 2


# global grand mean - average predicted outcome ignoring group deviations
# aka average marginal effect for Alpha_mode

conditional_effects(models_sub[[best_model_num]], re_formula = NULL)

### 
# TP_mode ~ log_mass * Alpha_mode --------
expand_grid(
    Alpha_mode = c(0.2, 0.8),
    log_mass = modelr::seq_range(df_mod$log_mass, n = 100),
    lake_region = NA, 
    year = 2015, 
    season = 1
    ) |>
  add_epred_draws(models_sub[[best_model_num]], re_formula = NULL) %>%  
  mutate(Alpha_mode = factor(Alpha_mode)) |>  
  ggplot(aes(x = exp(log_mass), group = Alpha_mode)) +
  stat_lineribbon(aes(y = .epred, color = Alpha_mode), .width = c(.95)) +
  scale_color_manual(values = c("green", "blue")) + 
  scale_fill_brewer(palette = "Greys") +
  ggnewscale::new_scale_color() + 
  geom_point(
    data = df_mod,
    aes(x = mass_g, y = TP_mode, color = Alpha_mode),
    size = 3, alpha = 0.7)  +
  scale_color_gradient(low="green", high = "blue") +
  scale_x_log10(labels = scales::label_comma()) +
  ggrepel::geom_text_repel(
    data = df_mod, aes(y = TP_mode, label = species),
    max.overlaps = 50, size=3) +
  labs(
    # title = "2015 data, baseline TP = 1, whole-lake scale",
    # subtitle = "Marginal effects - global grand mean",
    x = "Body mass (log)",
    y = "Trophic Position" 
    ) +
  theme_clean()


# TP_mode ~ Alpha_mode -----------
df_mod %>%
  as_tibble() |> 
  modelr::data_grid(
    Alpha_mode = modelr::seq_range(Alpha_mode, n = 100),
    lake_region = NA, 
    # year = 2015, 
    season = 1
  ) |>
  add_epred_draws(models_sub[[best_model_num]], re_formula = NULL) %>%  
  ggplot(aes(x = Alpha_mode)) +
  stat_lineribbon(aes(y = .epred), fill = "grey", .width = c(.95, .8, .5),alpha = 0.5) +
  geom_point(
    data = df_mod, 
    # aes(x = Alpha_mode, y = TP_mode, color = Alpha_mode),
    aes(x = Alpha_mode, y = TP_mode, color = lake_region),
    # aes(x = Alpha_mode, y = TP_mode, color = lake_region, shape = season),
    size = 3, alpha = 0.7)  +
  # scale_color_gradient(low="green", high = "blue") +
  scale_color_brewer(palette = "Set1") + 
  # ggrepel::geom_text_repel(
  #   data = df_mod, aes(y = TP_mode, label = species), 
  #   max.overlaps = 50, size=3) +
  labs(
    title = "2015 data, region-season scale, 1° producer baseline",
    subtitle = "Marginal effects - global grand mean",
    x = "Alpha",
    y = "Trophic Position",
    color = "Lake Region"
  ) +
  theme_clean()


# ggsave(here("out", "plots", "01a_2015_region_tp-alpha.png"),
#        width = 10, height = 8, dpi = 300)


# body mass ~ Alpha_mode -------------

df_mod %>%
  as_tibble() |> 
  modelr::data_grid(
    Alpha_mode = modelr::seq_range(Alpha_mode, n = 100),
    lake_region = "south", 
    year = 2015, 
    season = 1
  ) |>
  add_epred_draws(models_sub[[best_model_num]], re_formula = NULL) %>%  
  ggplot(aes(x = Alpha_mode)) +
  stat_lineribbon(aes(y = .epred), fill = "grey", .width = c(.95, .8, .5),alpha = 0.5) +
  # geom_point(
  #   data = df_mod, 
  #   # aes(x = Alpha_mode, y = log_mass, color = Alpha_mode),
  #   # aes(x = Alpha_mode, y = log_mass, color = lake_region),
  #   aes(x = Alpha_mode, y = exp(log_mass), color = lake_region),
  #   # aes(x = Alpha_mode, y = log_mass, color = lake_region, shape = season),
  #   size = 3, alpha = 0.7)  +
  # scale_color_gradient(low="green", high = "blue") +
  scale_color_brewer(palette = "Set1") + 
  # scale_y_log10() +
  # ggrepel::geom_text_repel(
  #   data = df_mod, aes(y = TP_mode, label = species), 
  #   max.overlaps = 50, size=3) +
  labs(
    title = "2015 data, region-season scale, 1° producer baseline",
    subtitle = "Marginal effects - global grand mean",
    x = "Alpha",
    y = "Body Size (log10)",
    color = "Lake Region"
  ) +
  theme_clean()

ggsave(here("out", "plots", "01a_2015_regionseason_mass-alpha.png"),
       width = 10, height = 8, dpi = 300)





# Conditional effects for existing groups	
df_mod %>%
  as_tibble() |> 
  modelr::data_grid(
    Alpha_mode = modelr::seq_range(Alpha_mode, n = 100), 
    grp = levels(df_mod$grp)
  ) |> 
  add_epred_draws(models_sub[[best_model_num]], re_formula = ~ (1 | grp)) %>%  
  ggplot(aes(x = Alpha_mode)) +
  stat_lineribbon(aes(y = .epred), .width = c(.95, .8, .5),
                  alpha = 0.5, color = clrs[1], fill = clrs[1]) +
  # geom_point(data = df_mod, aes(y = TP_mode), size = 1, alpha = 0.7) + 
  facet_wrap(vars(grp))




# Diagnostics =======================

# plot(models_sub[[best_model_num]])
pp_check(models_sub[[best_model_num]], ndraws=50)
pp_check(
  models_sub[[best_model_num]], 
  type = "error_scatter_avg_vs_x", 
  size = 1.1, x="Alpha_mode") +
  stat_smooth(se = FALSE)
pp_check(models_sub[[best_model_num]], type = "stat_2d")
pp_check(models_sub[[best_model_num]], type = "loo_pit")
pp_check(models_sub[[best_model_num]], type = "scatter_avg", nsamples = 100)

res_df <- models_sub[[best_model_num]]$data %>% 
  mutate(predict_y = predict(models_sub[[best_model_num]])[ , "Estimate"], 
         std_resid = residuals(models_sub[[best_model_num]], type = "pearson")[ , "Estimate"])
ggplot(res_df, aes(predict_y, std_resid)) + 
  geom_point(size = 0.8) + 
  stat_smooth(se = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_blank(), axis.line = element_line())


# Check the parameters associated with the random effects
forest(models_sub[[best_model_num]],sort=FALSE)
