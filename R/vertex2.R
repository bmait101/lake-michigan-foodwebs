
vert_data <- vertex_sampled |> 
  filter(model == "Fig 3.g") |>
  rename(Alpha_mode = value) |> 
  mutate(estimate__ = 6)



data <- reg_mod_data_tidy[["p5"]]
p <- conditional_effects(brm_mods_list[["brm_mods_3b_p5"]][[8]], re_formula = NA) 
plot(p, points = TRUE)

p$Alpha_mode |> 
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_line(linewidth = 1) + 
  geom_boxplot(data = vert_data, aes(x = Alpha_mode, y = estimate__), outliers = FALSE) + 
  geom_point(
    data = data, aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
    size = 3, color = point_col, shape = 21) +
  scale_fill_gradient(low = littoral, high = pelagic) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(.2, 6.5)) +
  labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
  theme_clean() + 
  # theme(legend.position = c(0.1, .85))+ 
  theme(legend.position = "none")


