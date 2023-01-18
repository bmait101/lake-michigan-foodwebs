
names(reg_mod_data_2015)


# Explore the relationships =================

# select best model number (1, 2, or 3) for summaries and diagnostics
best_model_num <- 1
best_model_num <- 2


# global grand mean - average predicted outcome ignoring group deviations
# aka average marginal effect for Alpha_mode

int_conditions <- list(
  Alpha_mode = setNames(c(.2, .8), c("Neashore", "Pelagic"))
)
p <- conditional_effects(models_sub[[1]], "log_mass:Alpha_mode", 
                         int_conditions = int_conditions) 
plot(p)

p$`log_mass:Alpha_mode` |> 
  ggplot(aes(x = exp(log_mass), y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = factor(Alpha_mode)), alpha = .5) +
  geom_line(aes(color = factor(Alpha_mode))) + 
  # geom_point(
  #   data = df_mod, aes(x = Alpha_mode, y = exp(log_mass), fill = Alpha_mode),
  #   size = 3, color = "black", shape = 21) +  
  scale_color_manual(values=c("green","blue")) +
  scale_fill_manual(values=c("green","blue")) +
  scale_x_log10(labels = scales::label_comma()) +
  labs(x = "Body mass (g)", y = "Trophic position") + 
  theme_clean()


p <- conditional_effects(models_sub[[1]], "Alpha_mode", re_formula = NULL) 
p$Alpha_mode |> 
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey90") +
  geom_line() + 
  geom_point(
    data = reg_mod_data_2015[["scascale01_ind"]], aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
    size = 3, color = "black", shape = 21) +
  scale_fill_gradient(low="green", high = "blue") +
  labs(x = "Alpha", y = "Trophic Position") + 
  theme_clean()


p <- conditional_effects(models_sub[[2]], "Alpha_mode",  re_formula = NULL) 
p$Alpha_mode |> 
  mutate(across(c(estimate__, lower__, upper__), exp)) |> 
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey90") +
  geom_line() + 
  geom_point(
    data = reg_mod_data_2015[["scale03a"]], aes(x = Alpha_mode, y = exp(log_mass), fill = Alpha_mode),
    size = 3, color = "black", shape = 21) +  
  scale_fill_gradient(low="green", high = "blue") +
  scale_y_log10(labels = scales::label_comma()) +
  labs(x = "Alpha", y = "Body mass (g)") + 
  theme_clean()
