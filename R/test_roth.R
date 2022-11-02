# Code adapted from Fred Keppeler
# Oct 21 2022

# 1) calculate trophic position and alpha values for each species collected in 
# different "areas" using the Two Baseline Full model approach; 
# 2) Run hierarchical bayesian models with brms to explore the association 
# between alpha and TP; 
# 3) Compare models using different metrics (R2, LOO); and 
# 4) Create some diagnostic plots.

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

# Load data
df_roth

df <- df_roth |> 
  select(area, species, d15N, d13C_norm, length_mm) |> 
  rename(d13C = d13C_norm) |>
  mutate(trophic = "consumer") |> 
  as.data.frame()

# make fake baseline data, same N, diff c
d13C <- c(rep(rnorm(5, -22, 0.5),4), rep(rnorm(5, -28, 0.5),4))
d15N <- c(rep(rnorm(5, 7, 0.5),4), rep(rnorm(5, 5, 0.5),4))
area <- rep(c(rep("NWM", 5),rep("NEM", 5),rep("SWM", 5),rep("SEM", 5)),2)
species <- c(rep("b2", 20), rep("b1", 20))
baselines <- data.frame(species, area, d15N, d13C) |> mutate(trophic = species)

# combine
df <- df |> bind_rows(baselines)

df |> 
  ggplot(aes(d13C, d15N, color = trophic)) + 
  geom_point() + 
  facet_wrap(vars(area))

ggsave(here("out", "plots", "roth-tpdata.png"),
       width = 8, height = 6, dpi = 300)


# Extract stable isotope data from a data frame
IsotopesList <- extractIsotopeData(
  df,
  b1 = "b1", 
  b2 =  "b2",
  baselineColumn = "trophic", 
  consumersColumn = "species",
  d13C = "d13C", 
  d15N = "d15N",
  groupsColumn="area"
  )


# Run two baseline model
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
  thin = 1
  ) #Increse n.iter, burnin and thin to ensure model convergence

#save (TP_model,file="TP_model.RData")
#load ("TP_model.RData")

# Summarize TP data
TP_data <- fromParallelTP (TP_model, get = "summary")
colnames(TP_data) <- c('model','area','species','TP_lower','TP_upper',
                       'TP_median','TP_mode','Alpha_lower','Alpha_upper',
                       'Alpha_median','Alpha_mode')

# Data for models: average body size, merge TP data, and log transform body mass
df_mod <- df %>%
  # filter(! species %in% c("Dreissenids", "Invertebrate")) |> 
  # filter(! species %in% c("Round goby", "Rainbow smelt", "Alewife")) |>
  filter (trophic == "consumer") %>%
  select (species, area, length_mm) %>%
  group_by(species, area) %>%
  summarise_all(mean) %>%
  left_join(TP_data, by=c("species","area")) %>%
  mutate (area = factor(area),
          Log_Biomass = log(length_mm)) %>%
  as.data.frame ()

# Plot data
df_mod |> 
  ggplot(aes(Alpha_mode, length_mm)) + 
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", color = "black") +
  geom_point(aes(fill = Alpha_mode), size = 3, shape = 21) + 
  scale_fill_gradient(low = "green", high = "blue", na.value = NA) +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50) +
  # labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
  theme_bw() + 
  theme(axis.text=element_text(size=13,colour = "black"), 
        axis.title=element_text(size=16), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black")) 

ggsave(here("out", "plots", "roth-tp-length.png"),
       width = 10, height = 7, dpi = 300)




# RUN MODELS
# Example exploring the relationship between TP and ALPHA

#Different fixed structure + random intercept component
model_structures <- list(
  bf(TP_mode ~ length_mm * Alpha_mode + (1|area)),   
  bf(TP_mode ~ length_mm + Alpha_mode + (1|area)),   
  bf(TP_mode ~ 1 + (1|area)),   
  
  bf(TP_mode ~ poly(Alpha_mode,2) + (1|area)),
  bf(TP_mode ~ poly(Alpha_mode,1) + (1|area)),
  bf(TP_mode ~ 1 + (1|area)),
  
  bf(length_mm ~ poly(Alpha_mode,2) + (1|area)),
  bf(length_mm ~ poly(Alpha_mode,1) + (1|area)),
  bf(length_mm ~ 1 + (1|area))
  )

# names(model_structures) <- c("Quadratic","Linear", "Null")
models <- list()
for (i in 1:length (model_structures)){
  
  models[[i]] <- brm (model_structures[[i]], 
                      cores=ncores, 
                      control = list(adapt_delta = 0.90, max_treedepth = 10),
                      df_mod, seed = 12345,iter = 2000,thin=1) #Increse n.iter, burnin and thin to ensure model convergence
}

#save(models,file="models_TP_alpha.R")
#load("models_TP_alpha.R")



# Model comparison =================

# targ_models <- models[1:3]
# targ_models <- models[4:6]
targ_models <- models[7:9]

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
  conditional_effects(targ_models[[best_model_num]]),
  points = TRUE,
  point_args = list(size=3, alpha=0.5)
  )


P1[[1]] + 
  geom_line(color='black',size=1.1) +
  theme(axis.text=element_text(size=13,colour = "black"), 
        axis.title=element_text(size=16), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black")) 



ggsave(here("out", "plots", "roth-tp-length-x.png"),
       width = 10, height = 7, dpi = 300)

# Diagnostics =======================

plot(targ_models[[best_model_num]])
pp_check(targ_models[[best_model_num]], ndraws=10)
pp_check(targ_models[[best_model_num]], type = "error_scatter_avg_vs_x", size = 1.1, x="length") +
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
