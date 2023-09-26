
## Model Diagnostics

# All statistical models are sets of assumptions about the data 
# generating process, and estimation will be meaningless or misleading 
# if theses assumptions do not hold for the data. 

# 1. Correct specification of the model (posterior predictive checks)
# 2. Linearity (Marginal mode plots, residuals)
# 3. Independence of observations
# 4. Equal variance of errors (standardized residuals)
# 5. Normality

## Prep
library(bayesplot)
color_scheme_set("brightblue")

names(reg_mod_data_tidy)
names(brm_mods_list_asym)
names(brm_mods_list)

mod_asym <- brm_mods_list_asym[["brm_mods_3b_s4_asym"]][[1]]
mod_asym <- brm_mods_list_asym[["brm_mods_3b_p5_asym"]][[1]]

mod_couple_3bs4 <- brm_mods_list[["brm_mods_3b_s4"]][[6]]  # best fit coupling model with specific baselines
mod_couple_3bp5 <- brm_mods_list[["brm_mods_3b_p5"]][[8]]  # best fit coupling model with pooled baselines

## Coupling - specific (3bs4) ==========================

print(mod_couple_3bs4)
# posterior_summary(mod_couple_3bs4)


### PP checks ====================================================================

#### Graphical check
pp_check(mod_couple_3bs4, type = "dens_overlay", ndraws=100)

# No major systematic discrepancies of our data 
# from what can be predicted from our model.
# Though, the peak for Yrep is a bit lower than Y. 

#### PPC for the mean (it should always fit)
# pp_check(mod_couple_3bs4, type = "stat_grouped", stat = "mean", group = "Alpha_mode")

#### PPC for the maximum and minimum values
pp_check(mod_couple_3bs4, type = "stat_2d", stat = c("max", "min"))

#### Ribbon plot to check for outliers
pp_check(mod_couple_3bs4, type = "ribbon", y_draw = "both")


### Residual plots =========================================

#### Residuals

pp_check(mod_couple_3bs4, type = "error_scatter_avg")  # Q-Q plot 
pp_check(mod_couple_3bs4, type = "error_scatter_avg_vs_x", x="Alpha_mode")

#### Standardized residuals

# There are no intrinsic way to standardize the residuals in Bayesian methods,
# and it’s not necessary to standardize them. However, it’s easier to 
# compare with frequentist results to flag cases with standardized
# residuals larger than 3 or 4 in standardized values. The plot below 
# shows the standardized residuals against the predicted y values.

# Get species and log mass values
tmp <- reg_mod_data_tidy[["s4"]] |> 
  select(species, log_mass)

# Pull residuals and bind tmp data
res_3bs4 <- mod_couple_3bs4$data |> 
  mutate(predict_y = predict(mod_couple_3bs4)[ , "Estimate"], 
         std_resid = residuals(mod_couple_3bs4)[ , "Estimate"])|> 
  as_tibble() |> 
  bind_cols(tmp)

# Q-Q plot; residuals vs fitted (look for non-linearity)
ggplot(res_3bs4, aes(predict_y, std_resid)) + 
  geom_point() + 
  stat_smooth(se = FALSE)

# Residuals vs. explanatory variables
p.resid.3bs4 <- res_3bs4 |> 
  ggplot(aes(Alpha_mode, std_resid, color = log_mass)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point() +
  labs(x="Alpha",y="Standardized residual",color="log mass",
       title = "Residuals vs. alpha (specific baselines)") + 
  theme_clean()
p.resid.3bs4

ggplot(res_3bs4, aes(TP_mode, std_resid)) +
  geom_point()

ggplot(res_3bs4, aes(lake_region, std_resid)) +
  geom_boxplot()

ggplot(res_3bs4, aes(season, std_resid)) +
  geom_boxplot()


### Multicollinearity ===============================

# Strictly speaking, multicollinearity is not an assumption of regression. 
# However, especially in frequentist analysis, having predictors that are 
# strongly correlated can increase the uncertainty of the posterior 
# distributions of the regression coefficients.

# If some coefficients are particularly strongly correlated, you may need 
# to think about using a stronger prior or combining some predictors.

# posterior density of the coefficients to see how correlated they are
pairs(mod_couple_3bs4, pars = "b", 
      off_diag_args =  # arguments of the scatterplots
        list(size = 0.5,  # point size
             alpha = 0.25))  # transparency


## Coupling - pooled (3bp5) ==========================

print(mod_couple_3bp5)
# posterior_summary(mod_couple_3bp5)

### PP checks ====================================================================

#### Graphical check
pp_check(mod_couple_3bp5, type = "dens_overlay", ndraws=100)

# No major systematic discrepancies of our data 
# from what can be predicted from our model.
# Though, the peak for Yrep is a bit lower than Y. 

#### PPC for the mean (it should always fit)
# pp_check(mod_couple_3bp5, type = "stat_grouped", stat = "mean", group = "Alpha_mode")

#### PPC for the maximum and minimum values
pp_check(mod_couple_3bp5, type = "stat_2d", stat = c("max", "min"))

#### Ribbon plot to check for outliers
pp_check(mod_couple_3bp5, type = "ribbon", y_draw = "both")

### Residual plots =========================================

#### Residuals

pp_check(mod_couple_3bp5, type = "error_scatter_avg")  # Q-Q plot 
pp_check(mod_couple_3bp5, type = "error_scatter_avg_vs_x", x="Alpha_mode")

#### Standardized residuals

# There are no intrinsic way to standardize the residuals in Bayesian methods,
# and it’s not necessary to standardize them. However, it’s easier to 
# compare with frequentist results to flag cases with standardized
# residuals larger than 3 or 4 in standardized values. The plot below 
# shows the standardized residuals against the predicted y values.

# Get species and log mass values
tmp <- reg_mod_data_tidy[["p5"]] |> 
  select(species, log_mass)

# Pull residuals and bind tmp data
res_3bp5 <- mod_couple_3bp5$data |> 
  mutate(predict_y = predict(mod_couple_3bp5)[ , "Estimate"], 
         std_resid = residuals(mod_couple_3bp5)[ , "Estimate"])|> 
  as_tibble() |> 
  bind_cols(tmp)

# Q-Q plot; residuals vs fitted (look for non-linearity)
ggplot(res_3bp5, aes(predict_y, std_resid)) + 
  geom_point() + 
  stat_smooth(se = FALSE)

# Residuals vs. explanatory variables
p.resid.3bp5 <- res_3bp5 |> 
  ggplot(aes(Alpha_mode, std_resid, color = log_mass)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point() + 
  labs(x="Alpha",y="Standardized residual",color="log mass", 
       title = "Residuals vs. alpha (pooled baselines)") + 
  theme_clean()
p.resid.3bp5

ggplot(res_3bp5, aes(TP_mode, std_resid)) +
  geom_point()

ggplot(res_3bp5, aes(lake_region, std_resid)) +
  geom_boxplot()

ggplot(res_3bp5, aes(season, std_resid)) +
  geom_boxplot()


### Multicollinearity ===============================

# Strictly speaking, multicollinearity is not an assumption of regression. 
# However, especially in frequentist analysis, having predictors that are 
# strongly correlated can increase the uncertainty of the posterior 
# distributions of the regression coefficients.

# If some coefficients are particularly strongly correlated, you may need 
# to think about using a stronger prior or combining some predictors.

# posterior density of the coefficients to see how correlated they are
pairs(res_3bp5, pars = "b", 
      off_diag_args =  # arguments of the scatterplots
        list(size = 0.5,  # point size
             alpha = 0.25))  # transparency


## Plots to go ===================

p.resid.panel <- p.resid.3bs4 / p.resid.3bp5
p.resid.panel <- p.resid.panel & 
  plot_annotation(tag_levels = "a")

path <- here::here("out", "plots", "residuals_coupling_panel")
ggsave(glue::glue("{path}.pdf"), plot = p.resid.panel, 
       width = 4, height = 4, scale = 1.5, device = cairo_pdf)
pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 300)
