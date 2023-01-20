# Diagnostics -------------------------------------

pp_check(models_sub[[3]], ndraws=100)
forest(models_sub[[1]],sort=FALSE)

names(brm_mods_2015)
mod <- brm_mods_2015[["scale03a"]][[8]]

pp_check(mod, ndraws=100)
pp_check(
  mod, 
  type = "error_scatter_avg_vs_x", 
  size = 1.1, x="Alpha_mode") +
  stat_smooth(se = FALSE)
pp_check(mod, type = "stat_2d")
pp_check(mod, type = "loo_pit")
pp_check(mod, type = "scatter_avg", nsamples = 100)

res_df <- mod$data %>% 
  mutate(predict_y = predict(mod)[ , "Estimate"], 
         std_resid = residuals(mod, type = "pearson")[ , "Estimate"])
ggplot(res_df, aes(predict_y, std_resid)) + 
  geom_point(size = 0.8) + 
  stat_smooth(se = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_blank(), axis.line = element_line())

