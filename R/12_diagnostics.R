
# Model Diagnostics

# All statistical models are sets of assumptions about the data 
# generating process, and estimation will be meaningless or misleading 
# if theses assumptions do not hold for the data. 

# 1. Correct specification of the model (posterior predictive checks)
# 2. Linearity (Marginal mode plots, residuals)
# 3. Independence of observations
# 4. Equal variance of errors (standardized residuals)
# 5. Normality

# Prep ==========================================
library(bayesplot)
color_scheme_set("brightblue")

available_ppc()

names(brm_mods_list_asym)
names(brm_mods_list)
names(reg_mod_data_tidy)

mod <- brm_mods_list_asym[["brm_mods_3b_p5_asym"]][[1]]
mod <- brm_mods_list[["brm_mods_3b_p5"]][[6]]
mod <- brm_mods_list[["brm_mods_3b_s4"]][[6]]

print(mod)
posterior_summary(mod)


# PP checks ====================================================================

# Graphical check ------------------------------------------
pp_check(mod, type = "dens_overlay", ndraws=100)

# no major systematic discrepancies of our data 
# from what can be predicted from our model

# PPC for the mean (it should always fit) ----------------------------------
# pp_check(mod, type = "stat_grouped", stat = "mean", group = "Alpha_mode")

# PPC for the maximum and minimum values ---------------------------------
pp_check(mod, type = "stat_2d", stat = c("max", "min"))


# Ribbon plot to check for outliers ------------------------------------
pp_check(mod, type = "ribbon", y_draw = "both")


# Residual plots =========================================

# Residuals -------------------------

# If the model fit the data well, the points should be 
# scattered with no specific pattern. If you see that the SD of the residuals 
# is not uniform, or the residuals have some non-linear relationships 
# with the predictor, there can be some problems.

pp_check(mod, type = "error_scatter_avg")  # Q-Q plot (residuals are normally distributed?)
pp_check(mod, type = "error_scatter_avg_vs_x", x="Alpha_mode")
pp_check(mod, type = "error_scatter_avg_vs_x", x="log_mass")


# Standardized residuals -------------------------

# There are no intrinsic way to standardize the residuals in Bayesian methods,
# and it’s not necessary to standardize them. However, it’s easier to 
# compare with frequentist results to flag cases with standardized
# residuals larger than 3 or 4 in standardized values. The plot below 
# shows the standardized residuals against the predicted y values.

# Pull residuals
res_df <- mod$data %>% 
  mutate(predict_y = predict(mod)[ , "Estimate"], 
         std_resid = residuals(mod)[ , "Estimate"])

# Residuals vs fitted (look for non-linearity)
ggplot(res_df, aes(predict_y, std_resid)) + 
  geom_point() + 
  stat_smooth(se = FALSE)

# Some funneling is apparent in variances

# Residuals vs. explanatory variables
ggplot(res_df, aes(Alpha_mode, std_resid)) + 
  geom_point() 

ggplot(res_df, aes(TP_mode, std_resid)) +
  geom_point()

ggplot(res_df, aes(lake_region, std_resid)) +
  geom_boxplot()

ggplot(res_df, aes(season, std_resid)) +
  geom_boxplot()


# Multicollinearity ===============================

# Strictly speaking, multicollinearity is not an assumption of regression. 
# However, especially in frequentist analysis, having predictors that are 
# strongly correlated can increase the uncertainty of the posterior 
# distributions of the regression coefficients.

# If some coefficients are particularly strongly correlated, you may need 
# to think about using a stronger prior or combining some predictors.

# posterior density of the coefficients to see how correlated they are
pairs(mod, pars = "b", 
      off_diag_args =  # arguments of the scatterplots
        list(size = 0.5,  # point size
             alpha = 0.25))  # transparency



# Other PP checks =====================================================
 
pp_check(mod, type = "dens_overlay", ndraws=100)
# pp_check(mod, type = "hist", ndraws = 11)

# pp_check(mod, type = "scatter", ndraws = 10)
pp_check(mod, type = "scatter_avg", ndraws = 100)  # Q-Q plot (residuals are normally distributed?)


# pp_check(mod, type = "intervals", ndraws = 100)
pp_check(mod, type = "intervals", ndraws = 100, size = 1.5, fatten = 0)
pp_check(mod, type = "intervals", ndraws = 100, size = 1.5, fatten = 0,  x = "Alpha_mode")
pp_check(mod, type = "intervals", ndraws = 100, size = 1.5, fatten = 0,  x = "log_mass")

# pp_check(mod, type = "stat")
# pp_check(mod, type = "stat_2d")
# pp_check(mod, type = "stat_freqpoly", ndraws = 100)

# LOO PP checks ==========================================================
pp_check(mod, type = "loo_intervals", ndraws = 100)
pp_check(mod, type = "loo_pit_overlay", ndraws = 100)
pp_check(mod, type = "loo_pit", ndraws = 100)
pp_check(mod, type = "loo_pit_qq", ndraws = 100)
pp_check(mod, type = "loo_ribbon", ndraws = 100)



