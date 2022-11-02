
# Load packages
#remotes::install_github("mvuorre/brmstools")
pacman::p_load(
  parallel, 
  dplyr, 
  ggplot2,
  tRophicPosition, 
  brms, 
  rstanarm, 
  brmstools, 
  rstan
)

# for parallel processing
cl <- parallel::makePSOCKcluster(parallel::detectCores())
ncores <- 5

# Source function
source("R/r2_bayes.R")


## Prep data
df <- df_csmi |> 
  # filter(site_code %in% c("RAC")) |>
  filter(! site_code %in% c("SAU")) |>
  # unite(area, site_code, depth_m) |> 
  # filter(! area %in% c("WAK_46")) |>
  # filter(depth_m %in% c("2", "18", "46")) |> 
  # filter(! (depth_m=="2" & species != "Algae")) |> # removes inverts and fish from 2 m
  rename(area = site_code) |> 
  select(area, species, d15N, d13C_norm) |> 
  rename(d13C = d13C_norm) |>
  droplevels() |> 
  mutate(trophic = "consumer") |> 
  mutate(trophic = ifelse(species=="POM", "b1", trophic)) |> 
  mutate(trophic = ifelse(species=="Algae", "b2", trophic)) |> 
  as.data.frame()

# make fake algae data
# alg <- df_csmi |> 
#   filter(depth_m == "2", species == "Algae") |> 
#   select(site_code, d13C_norm, d15N) |> 
#   group_by(site_code) |> 
#   summarise_all(list(mean = mean, sd = sd))
# 
# df_alg <- df |> 
#   distinct(area) |> pull() |> as_tibble() |> rename(area=value) |> 
#   mutate(species = "Algae", trophic = "b2") |> 
#   separate(area, into = c("site_code", "depth_m")) |> 
#   left_join(alg, by = "site_code") |> 
#   unite(area, site_code, depth_m) |> 
#   select(-d13C_norm_sd, -d15N_sd) |> 
#   rename(d13C = d13C_norm_mean, d15N = d15N_mean)
# 
# df <- df |> 
#   bind_rows(df_alg) 

df |> 
  ggplot(aes(d13C, d15N)) + 
  geom_point(aes(shape = trophic, color = trophic), size = 3) + 
  facet_wrap(vars(area)) +
  labs(color = "Trophic Group", shape = "Trophic Group")

ggsave(here("out", "plots", "csmi-2-110m-data.png"),
       width = 12, height = 10, dpi = 300)


## Calculate TP and alpha

# Extract stable isotope data from a data frame
IsotopesList <- extractIsotopeData(
  df,
  b1 = "b1",
  b2 = "b2", 
  baselineColumn = "trophic", 
  consumersColumn = "species",
  d13C = "d13C", 
  d15N = "d15N",
  groupsColumn="area"
  )

# Run two baseline model (Increse n.iter, burnin, thin 4 model convergence)
TP_model <- parLapply(
  cl, 
  IsotopesList, 
  multiModelTP,
  model = "twoBaselinesFull",
  lambda = 1,
  n.chains = 5, 
  print = TRUE,
  n.iter = 1000, 
  burnin = 100, 
  thin=1
  ) 

#save (TP_model,file="TP_model.RData")
#load ("TP_model.RData")

# Summarize TP data
TP_data <- fromParallelTP(TP_model, get = "summary")
colnames(TP_data) <- c(
  'model','area','species',
  'TP_lower','TP_upper','TP_median','TP_mode',
  'Alpha_lower','Alpha_upper','Alpha_median','Alpha_mode'
  )

# Data for models:
df_mod <- df %>%
  filter(trophic == "consumer") %>%
  select(species, area) %>%
  group_by(species, area) %>%
  summarise_all(mean) %>%
  left_join(TP_data, by=c("species","area")) %>%
  # separate(area, into = c("site_code", "depth_m")) |> 
  mutate(
    area = factor(area)) %>%
  # mutate(
  #   site_code = factor(site_code), 
  #   depth_m = factor(depth_m)
  #   ) %>%
  as.data.frame ()

# Plot data
df_mod |> 
  ggplot(aes(Alpha_mode, TP_mode)) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", color = "black") +
  geom_point(aes(fill = Alpha_mode), size = 3, shape = 21) + 
  scale_fill_gradient(low = "green", high = "blue", na.value = NA) +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50) +
  labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
  theme_bw() + 
  theme(axis.text=element_text(size=13,colour = "black"), 
        axis.title=element_text(size=16), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black")) 

ggsave(here("out", "plots", "csmi-2-110m-tp-alpha.png"),
       width = 10, height = 7, dpi = 300)


# RUN MODELS

#Different fixed structure + random intercept component
model_structures <- list(
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|area)),  # Quadratic
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|area)),  # Linear
  bf(TP_mode ~ 1 + (1|area))                   # Null
  )

# names(model_structures) <- c("Quadratic","Linear", "Null")
models <- list()
for (i in 1:length (model_structures)){
  models[[i]] <- brm(
    model_structures[[i]], 
    cores=ncores, 
    control = list(
      adapt_delta = 0.90, 
      max_treedepth = 10
      ),
    df_mod, 
    seed = 12345,
    iter = 2000,
    thin = 1
    ) #Increase n.iter, burnin and thin to ensure model convergence
}

#save(models,file="models_TP_alpha.R")
#load("models_TP_alpha.R")

# Model comparison =================

targ_models <- models[1:3]
# targ_models <- models[4:6]
# targ_models <- models[7:9]

#compare the different models using different metrics elpd, looic, r2
comp <- loo_compare (loo(targ_models[[1]]),
                     loo(targ_models[[2]]),
                     loo(targ_models[[3]]))
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
for (i in 1:length(targ_models)){
  r2_marg_cond <- r2_bayes(get("targ_models")[[i]])
  model_sel_tab[i,"r2_loo"] <- loo_R2 (get("targ_models")[[i]]) %>% round(digits=2)
  model_sel_tab[i,"r2_marg"] <- paste (r2_marg_cond$R2_Bayes_marginal %>% round(digits=2)," (",
                                       attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),
                                       ")",sep="")
  model_sel_tab[i,"r2_cond"] <- paste (r2_marg_cond$R2_Bayes %>% round(digits=2)," (",
                                       attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),
                                       ")",sep="")
  # count <- count + 1
}

model_sel_tab



# Explore the relationships =================

# select best model number (1, 2, or 3) for summaries and diagnostics
best_model_num <- 1

# int_conditions <- list(
#   Alpha_mode = setNames(c(.2,.8), c("littoral", "pelagic"))
# )

P1 <- plot(
  conditional_effects(
    targ_models[[best_model_num]]),
  points=TRUE,
  point_args = list(size=3, alpha=0.5)
)

# P1 <- plot(
#   conditional_effects(
#     targ_models[[best_model_num]],"length_mm:Alpha_mode", 
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



# Diagnostics =======================

# plot(targ_models[[best_model_num]])
pp_check(targ_models[[best_model_num]], ndraws=50)
pp_check(
  targ_models[[best_model_num]], 
  type = "error_scatter_avg_vs_x", 
  size = 1.1, x="length") +
  stat_smooth(se = FALSE)
pp_check(targ_models[[best_model_num]], type = "stat_2d")
pp_check(targ_models[[best_model_num]], type = "loo_pit")
pp_check(targ_models[[best_model_num]], type = "scatter_avg", nsamples = 100)

res_df <- targ_models[[best_model_num]]$data %>% 
  mutate(predict_y = predict(targ_models[[best_model_num]])[ , "Estimate"], 
         std_resid = residuals(targ_models[[best_model_num]], type = "pearson")[ , "Estimate"])
ggplot(res_df, aes(predict_y, std_resid)) + 
  geom_point(size = 0.8) + 
  stat_smooth(se = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_blank(), axis.line = element_line())


# Check the parameters associated with the random effects
forest(targ_models[[best_model_num]],sort=FALSE)
