
# Prep ====================

source(here::here("R", "00_prep.R"))

load(file = here("out", "data", "reg_mod_data_2015.RData"))
load(file = here("out", "models", "brms", "brm_mods_2015.RData"))

names(reg_mod_data_2015)
names(brm_mods_2015)

brm_mods_2015[[1]] # this is list each is a model object, which is a list
str(brm_mods_2015[[1]][1]) # returns list of 1, which is list of 22
str(brm_mods_2015[[1]][[1]]) # returns the model object, which is a list


# Model comparison 2015 =================

# compare the different models using different metrics elpd, looic, r2
# List 5 and 8 are the 'b' analyses (pooled baselines, estimated by grouping)


# Scale 1
models_sub <- brm_mods_2015[[1]][1:3]
models_sub <- brm_mods_2015[[1]][4:6]
models_sub <- brm_mods_2015[[1]][7:9]
# Scale 1 - Ind
models_sub <- brm_mods_2015[[2]]

# Scale 2
models_sub <- brm_mods_2015[[3]][1:5]
models_sub <- brm_mods_2015[[3]][6:10]
models_sub <- brm_mods_2015[[3]][11:15]
# Scale 2 - Ind
models_sub <- brm_mods_2015[[4]]

# Scale 3
models_sub <- brm_mods_2015[[6]][1:5]
models_sub <- brm_mods_2015[[6]][6:10]
models_sub <- brm_mods_2015[[6]][11:15]
# Scale 3 - Ind
models_sub <- brm_mods_2015[[7]]


comp <- loo_compare(
  loo(models_sub[[1]]),
  loo(models_sub[[2]]),
  loo(models_sub[[3]])
)

comp <- loo_compare(
  loo(models_sub[[1]]),
  loo(models_sub[[2]]),
  loo(models_sub[[3]]), 
  loo(models_sub[[4]]), 
  loo(models_sub[[5]])
)

comp_summary <-
  print(comp, simplify = FALSE, digits = 3) %>%
  as.data.frame()

model_sel_tab <- data.frame(
  elpd_diff = rep(NA, nrow(comp_summary)),
  elpd = rep(NA, nrow(comp_summary)),
  p_loo = rep(NA, nrow(comp_summary)),
  looic = rep(NA, nrow(comp_summary)),
  r2_loo = rep(NA, nrow(comp_summary)),
  r2_marg = rep(NA, nrow(comp_summary)),
  r2_cond = rep(NA, nrow(comp_summary)),
  row.names = rownames(comp_summary)
  )

model_sel_tab$elpd_diff <- paste (comp_summary$elpd_diff %>% round(digits=2), " (",
                                  comp_summary$se_diff %>% round(digits=2), ")",sep="")
model_sel_tab$elpd <- paste (comp_summary$elpd_loo %>% round(digits=2), " (",
                             comp_summary$se_elpd_loo %>% round(digits=2), ")",sep="")
model_sel_tab$p_loo <- paste (comp_summary$p_loo %>% round(digits=2)," (",
                              comp_summary$se_p_loo %>% round(digits=2), ")",sep="")
model_sel_tab$looic <- paste (comp_summary$looic %>% round(digits=2)," (",
                              comp_summary$se_looic %>% round(digits=2), ")",sep="")

for (i in 1:length(models_sub)){
  r2_marg_cond <- r2_bayes(models_sub[[i]])
  model_sel_tab[i,"r2_loo"] <- loo_R2(models_sub[[i]]) %>% round(digits=2)
  # model_sel_tab[i,"r2_marg"] <- paste (r2_marg_cond$R2_Bayes_marginal %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),")",sep="")
  model_sel_tab[i,"r2_cond"] <- paste (r2_marg_cond$R2_Bayes %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),")",sep="")
}

model_sel_tab

# Dignostics -------------------------------------

pp_check(models_sub[[3]], ndraws=100)
forest(models_sub[[1]],sort=FALSE)
