
# Load packages
#remotes::install_github("mvuorre/brmstools")
pacman::p_load(
  parallel, 
  dplyr, ggplot2,
  tRophicPosition, 
  brms, brmstools, 
  rstan,rstanarm
)

# for parallel processing
cl <- parallel::makePSOCKcluster(parallel::detectCores())
ncores <- 5

# Source function
source("R/r2_bayes.R")


## Prep data ============

df <- data |> 
  select(dataset, lake_region, year, season, compartment, species, d13c_norm, d15n, length_mm) |> 
  drop_na(d13c_norm, d15n, lake_region, year, season) |> 
  # filter(dataset %in% c("csmi_2015", "nps_2015")) |>
  filter(dataset %in% c("csmi_2015", "nps_2015","glft_2016","roth_2019")) |>
  filter(!compartment %in% c("ichthoplankton")) |>
  # filter(!compartment %in% c("ichthoplankton", "pom", "macro-alga")) |>
  # mutate(grp = "pooled") |>
  mutate(grp = lake_region) |>
  # mutate(grp = season) |>
  # unite("grp", lake_region, season) |>
  # filter(! grp %in% c("ne_1", "se_1")) |>
  # unite("species", c(species, lake_region)) |>
  # unite("species", c(species, season)) |>
  # unite("species", c(species, lake_region, season)) |>
  mutate(
    trophic = case_when(
      compartment == "pom" ~ "b1",
      compartment == "macro-alga" ~ "b2",
      TRUE ~ "consumer"
  )) |> 
  droplevels() |> 
  as.data.frame()

df |> 
  ggplot(aes(d13c_norm, d15n)) + 
  geom_point(aes(color = trophic), size = 2, alpha = .5) + 
  facet_wrap(vars(grp)) +
  labs(color = "Trophic Group", shape = "Trophic Group")


# ggsave(here("out", "plots", "2015-19_tp-data_corr-fish.png"),
#        width = 7, height = 5, dpi = 300)


## TP and alpha ===============

IsotopesList <- extractIsotopeData(
  df,
  b1 = "b1",
  b2 = "b2", 
  baselineColumn = "trophic", 
  consumersColumn = "species",
  groupsColumn = "grp",
  d13C = "d13c_norm", 
  d15N = "d15n"
  )

summary(IsotopesList)
plot(IsotopesList$`nw-alewife_nw_2`)


# Run two baseline model (Increase n.iter, burn in, thin 4 model convergence)
TP_model_5_d <- parLapply(
  cl, 
  IsotopesList, 
  multiModelTP,
  model = "twoBaselinesFull",
  lambda = 1,
  n.chains = 5, 
  print = TRUE,
  n.iter = 1000, 
  burnin = 100, 
  thin = 1
  ) 


TP_model <- TP_model_5_b


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

# Data for models:
df_mod <- df %>%
  filter(trophic == "consumer") %>%
  select(species, length_mm) %>%
  group_by(species) %>%
  summarise(length_mm = mean(length_mm, na.rm = TRUE)) %>%
  left_join(TP_data, by=c("species")) %>%
  mutate(group = factor(group)) %>%
  # separate(species, into = c("species", "lake_region"), sep = "_") |>
  # separate(species, into = c("species", "season"), sep = "_") |>
  # separate(species, into = c("species", "lake_region", "season"), sep = "_") |>
  # unite("region_season", lake_region, season) |> 
  # mutate(group = factor(season)) %>%
  as.data.frame ()

# Plot data
df_mod |> 
  ggplot(aes(Alpha_mode, TP_mode)) +
  # ggplot(aes(Alpha_mode, length_mm)) +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", color = "black") +
  geom_point(aes(fill = group), size = 3, alpha = 0.5, shape = 21) +
  # ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=2) +
  scale_fill_viridis_d() +
  labs(title = "2015-2019, region baseline, by season",x = "Alpha", y = "TP") +
  xlim(c(0,1)) +  
  ylim(c(0.5,5)) + 
  theme_bw() + 
  theme(axis.text=element_text(size=14,colour = "black"), 
        axis.title=element_text(size=16), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black")) 

# ggsave(here("out", "plots", "05_d.png"),
#        width = 12, height = 8, dpi = 300)



# RUN MODELS ===================================================================

# Different fixed structure + random intercept component

model_structures <- list(
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|year)),  # Quadratic
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|year)),  # Linear
  bf(TP_mode ~ 1 + (1|year))                   # Null
  )

model_structures <- list(
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|group)),  # Quadratic
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|group)),  # Linear
  bf(TP_mode ~ 1 + (1|group))                   # Null
)

# model_structures <- list(
#   bf(TP_mode ~ length_mm * Alpha_mode + (1|group)),
#   bf(TP_mode ~ length_mm + Alpha_mode + (1|group)),
#   bf(TP_mode ~ 1 + (1|group)),
# 
#   bf(TP_mode ~ poly(Alpha_mode,2) + (1|group)),
#   bf(TP_mode ~ poly(Alpha_mode,1) + (1|group)),
#   bf(TP_mode ~ 1 + (1|group)),
# 
#   bf(length_mm ~ poly(Alpha_mode,2) + (1|group)),
#   bf(length_mm ~ poly(Alpha_mode,1) + (1|group)),
#   bf(length_mm ~ 1 + (1|group))
# )

# Fit models to data
names(model_structures) <- c("Quadratic","Linear", "Null")
models <- list()
for (i in 1:length (model_structures)){
  models[[i]] <- brm(
    model_structures[[i]],
    cores = ncores,
    control = list(adapt_delta = 0.90, max_treedepth = 10),
    df_mod, seed = 12345, iter = 2000, thin = 1
    )
}

# save models
# save(models, file="models_TP_alpha.R")
# load("models_TP_alpha.R")



# Model comparison =================

models_sub <- models[1:3]
models_sub <- models[4:6]
models_sub <- models[7:9]

# compare the different models using different metrics elpd, looic, r2
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

# int_conditions <- list(
#   Alpha_mode = setNames(c(.2,.8), c("littoral", "pelagic"))
# )

conditional_effects(models_sub[[best_model_num]])

P1 <- plot(
  conditional_effects(
    models_sub[[best_model_num]]),
  points=TRUE,
  point_args = list(size=3, alpha=0.5)
)

# P1 <- plot(
#   conditional_effects(
#     models_sub[[best_model_num]],"length_mm:Alpha_mode", 
#     int_conditions = int_conditions),
#   points=TRUE,
#   point_args = list(size=3, alpha=0.5)
# )

P1[[1]] + 
  geom_line(color='black',size=1.1) +
  theme(axis.text=element_text(size=13,colour = "black"), 
        axis.title=element_text(size=16), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black")) 

ggsave(here("out", "plots", "csmi_tp-alpha_modeled.png"),
       width = 8, height = 6, dpi = 300)
# ggsave(here("out", "plots", "csmi_length-alpha_modeled.png"),
#        width = 8, height = 6, dpi = 300)
# ggsave(here("out", "plots", "csmi_length-alpha_x_modeled.png"),
#        width = 8, height = 6, dpi = 300)



# Diagnostics =======================

# plot(models_sub[[best_model_num]])
pp_check(models_sub[[best_model_num]], ndraws=50)
pp_check(
  models_sub[[best_model_num]], 
  type = "error_scatter_avg_vs_x", 
  size = 1.1, x="length") +
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
