
library(ggnewscale)


# custom plotting theme
theme_clean <- function() {
  theme_classic(base_size = 16) 
}


# Global plot settings

pointsize = 4
linesize = 1
point_col = "black"
littoral = "green"
pelagic = "blue"
ribbon_col = "darkgrey"

# Explore the relationships =================

names(brm_mods_2015)
names(reg_mod_data_2015)

# global grand mean - average predicted outcome ignoring group deviations
# aka average marginal effect for Alpha_mode

int_conditions <- list(
  Alpha_mode = setNames(c(.2, .8), c("Neashore", "Pelagic"))
)
p <- conditional_effects(brm_mods_2015[["scale01"]][[1]], "log_mass:Alpha_mode", 
                         int_conditions = int_conditions, re_formula = NULL) 
# plot(p, points = TRUE)
p1 <- p$`log_mass:Alpha_mode` |> 
  ggplot(aes(x = exp(log_mass), y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = factor(Alpha_mode)), alpha = .2, show.legend = FALSE) +
  geom_line(aes(color = factor(Alpha_mode)), size = linesize, show.legend = FALSE) + 
  scale_color_manual(values=c(littoral, pelagic)) +
  scale_fill_manual(values=c(littoral, pelagic)) +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = reg_mod_data_2015[["scale01"]], aes(x = mass_g, y = TP_mode, fill = Alpha_mode),
    size = pointsize, shape = 21, color = point_col) +
  scale_fill_gradient(low = littoral, high = pelagic) +
  scale_x_log10(labels = scales::label_comma()) +
  coord_cartesian(ylim = c(1, max(reg_mod_data_2015[["scale01"]]$TP_mode) + 0.5)) +
  labs(title = "Asymmetric TP-body size relationship", 
       x = "Body mass (g)", y = "Trophic position", fill = "Alpha") + 
  theme_clean() + 
  theme(legend.position = c(0.08, .85))


p <- conditional_effects(brm_mods_2015[["scale01"]][[4]], "Alpha_mode", re_formula = NULL) 
# plot(p, points = TRUE)
p2 <- p$Alpha_mode |> 
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_line(size = linesize) + 
  geom_point(
    data = reg_mod_data_2015[["scale01"]], aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
    size = pointsize, color = point_col, shape = 21) +
  scale_fill_gradient(low = littoral, high = pelagic) +
  coord_cartesian(ylim = c(.8, 5)) +
  labs(title = "Coupling of energy pathways - TP (spp. level)", 
       x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
  theme_clean() + 
  theme(legend.position = c(0.1, .85))

p <- conditional_effects(brm_mods_2015[["scale01_ind"]][[1]], "Alpha_mode", re_formula = NULL) 
# plot(p, points = TRUE)
p3 <- p$Alpha_mode |> 
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_point(
    data = reg_mod_data_2015[["scascale01_ind"]], aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
    size = pointsize, color = point_col, shape = 21) +
  geom_line(size = linesize) + 
  scale_fill_gradient(low = littoral, high = pelagic) +
  coord_cartesian(ylim = c(.8, 5)) +
  labs(title = "Coupling of energy pathways - TP (ind. level)", 
       x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
  theme_clean() + 
  theme(legend.position = "none")


p <- conditional_effects(brm_mods_2015[["scale01"]][[7]], "Alpha_mode",  re_formula = NULL) 
p4 <- p$Alpha_mode |> 
  mutate(across(c(estimate__, lower__, upper__), exp)) |>
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_point(
    data = reg_mod_data_2015[["scale01"]], aes(x = Alpha_mode, y = mass_g, fill = Alpha_mode),
    size = pointsize, color = point_col, shape = 21) +
  geom_line(size = linesize) + 
  scale_fill_gradient(low = littoral, high = pelagic) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(title = "Coupling of energy pathways - size (spp. level)", 
       x = "Alpha", y = "Body mass (g)") + 
  theme_clean() + 
  theme(legend.position = "none")


# Compose figure
p1

(p2 | p3 | p4) + 
  plot_annotation(tag_levels = 'A')

