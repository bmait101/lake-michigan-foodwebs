# 1) calculate trophic position and alpha values for each species collected in 
    # different "areas" using the Two Baseline Full model approach; 
# 2) Run hierarchical bayesian models with brms to explore the association 
    # between alpha and TP; 
# 3) Compare models using different metrics (R2, LOO); and 
# 4) Create some diagnostic plots.

# Load packages
#remotes::install_github("mvuorre/brmstools")
library(tRophicPosition)
library(parallel)
library(dplyr)
library(brms)
library(rstanarm)
library(rstantools)
library(brmstools)
library(rstan)
library(ggplot2)

# for parallel processing
cl <- parallel::makePSOCKcluster(parallel::detectCores())
ncores <- 5

# Source function
source("r2_bayes.R")

# Load data
BaratariaBay <- read.csv("example_BaratariaBay.csv", header = T, sep=",")

# Extract stable isotope data from a data frame
IsotopesList <- extractIsotopeData(BaratariaBay,
                                   b1 = "Phyto", 
                                   b2 =  "Marsh",
                                   baselineColumn = "Taxon_group", 
                                   consumersColumn = "Species",
                                   d13C = "d13C", d15N = "d15N",
                                   groupsColumn="Area")


# Run two baseline model
TP_model <- parLapply(cl, IsotopesList, multiModelTP,
                      model = "twoBaselinesFull",
                      n.chains = 5, print = TRUE,lambda = 1,
                      n.iter = 1000, burnin = 100, thin=1) #Increse n.iter, burnin and thin to ensure model convergence

#save (TP_model,file="TP_model.RData")
#load ("TP_model.RData")

# Summarize TP data
TP_data <- fromParallelTP (TP_model, get = "summary")
colnames(TP_data) <- c('model','Area','Species','TP_lower','TP_upper',
                           'TP_median','TP_mode','Alpha_lower','Alpha_upper',
                           'Alpha_median','Alpha_mode')

# Data for models: average body size, merge TP data, and log transform body mass
df <- BaratariaBay %>%
  filter (Taxon_group == "Consumer") %>%
  select (Species, Area, Biomass_Pred_g) %>%
  group_by(Species, Area) %>%
  summarise_all(mean) %>%
  left_join(TP_data, by=c("Species","Area")) %>%
  mutate (Area = factor(Area),
          Log_Biomass = log(Biomass_Pred_g)) %>%
  as.data.frame ()

#RUN MODELS
# Example exploring the relationship between TP and ALPHA

#Different fixed structure + random intercept component
model_structures <- list(bf (TP_mode ~ poly(Alpha_mode,2) + (1|Area)), #Quadratic
                         bf (TP_mode ~ poly(Alpha_mode,1) + (1|Area)), #Linear
                         bf (TP_mode ~ 1 + (1|Area)) # Null
                         )
names(model_structures) <- c("Quadratic","Linear", "Null")
models <- list()
for (i in 1:length (model_structures)){
  
  models[[i]] <- brm (model_structures[[i]], 
                     cores=ncores, 
                     control = list(adapt_delta = 0.90, max_treedepth = 10),
                     df, seed = 12345,iter = 2000,thin=1) #Increse n.iter, burnin and thin to ensure model convergence
  }

# save(models,file="models_TP_alpha.R")
#load("models_TP_alpha.R")

#compare the different models using different metrics elpd, looic, r2
comp <- loo_compare (loo(models[[1]]),
                     loo(models[[2]]),
                     loo(models[[3]]))
comp_summary <- print(comp, simplify = FALSE, digits = 3) %>%
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
for (i in 1:length(models)){
  r2_marg_cond <- r2_bayes(get("models")[[i]])
  model_sel_tab[i,"r2_loo"] <- loo_R2 (get("models")[[i]]) %>% round(digits=2)
  model_sel_tab[i,"r2_marg"] <- paste (r2_marg_cond$R2_Bayes_marginal %>% round(digits=2)," (",
                                           attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),
                                           ")",sep="")
  model_sel_tab[i,"r2_cond"] <- paste (r2_marg_cond$R2_Bayes %>% round(digits=2)," (",
                                           attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),
                                           ")",sep="")
  # count <- count + 1
}

model_sel_tab

#Explore the relationship
P1<-plot (conditional_effects(models[[1]]),points=TRUE,
          point_args = list(size=3, alpha=0.5))
P1[[1]] + geom_line (color='black',size=1.1)+
  theme(axis.text=element_text(size=13,colour = "black"), axis.title=element_text(size=16), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))+
  ylab ('Trophic position')+ xlab ('Alpha')


#Check the parameters associated with the random effects
forest(models[[1]],sort=FALSE)

#Diagnostics
plot(models[[1]])
pp_check(models[[1]], ndraws=10)
pp_check(models[[1]], type = "error_scatter_avg_vs_x", size = 1.1, x="Alpha_mode")+
  stat_smooth(se = FALSE)
res_df <- models[[1]]$data %>% 
  mutate(predict_y = predict(models[[1]])[ , "Estimate"], 
         std_resid = residuals(models[[1]], type = "pearson")[ , "Estimate"])
ggplot(res_df, aes(predict_y, std_resid)) + 
  geom_point(size = 0.8) + stat_smooth(se = FALSE)+
  theme(	panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "white", colour = "black"),
         panel.border = element_blank(), axis.line = element_line())
pp_check(models[[1]], type = "stat_2d")
pp_check(models[[1]], type = "loo_pit")
pp_check(models[[1]], type = "scatter_avg", nsamples = 100)



