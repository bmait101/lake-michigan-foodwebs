


# Model comparison =================

# compare the different models using different metrics elpd, looic, r2

models_sub <- brm_mods_2015_01[1:3]
models_sub <- brm_mods_2015_01[4:6]
models_sub <- brm_mods_2015_01[7:9]

models_sub <- brm_mods_2015_02[1:5]
models_sub <- brm_mods_2015_02[6:10]
models_sub <- brm_mods_2015_02[11:15]

models_sub <- brm_mods_2015_03[1:5]
models_sub <- brm_mods_2015_03[6:10]
models_sub <- brm_mods_2015_03[11:15]

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
# r2_bayes(models_sub[[1]]) %>% round(digits=2)
# r2_marg_cond <- r2_bayes(models_sub[[1]])
# r2_marg_cond$R2_Bayes %>% round(digits=2)
# 
# loo_R2(models_sub[[3]]) %>% round(digits=2)
# loo_R2(get("models_sub")[[3]]) %>% round(digits=2)

for (i in 1:length(models_sub)){
  r2_marg_cond <- r2_bayes(models_sub[[i]])
  model_sel_tab[i,"r2_loo"] <- loo_R2(models_sub[[i]]) %>% round(digits=2)
  model_sel_tab[i,"r2_marg"] <- 
    # NA
  paste (r2_marg_cond$R2_Bayes_marginal %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),")",sep="")
  model_sel_tab[i,"r2_cond"] <- 
    paste (r2_marg_cond$R2_Bayes %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),")",sep="")
}

model_sel_tab


conditional_effects(models_sub[[1]], re_formula = NULL)
conditional_effects(models_sub[[3]], re_formula = NULL)


pp_check(models_sub[[1]], ndraws=100)
forest(models_sub[[1]],sort=FALSE)
