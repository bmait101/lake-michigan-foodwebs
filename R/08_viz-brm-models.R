
# Prep and data
source(here::here("R", "00_prep.R"))

# Model objects
load(file = here("out", "models", "brms", "brm_mods_2015.RData"))
# Data for plotting
load(file = here("out", "data", "reg_mod_data_2015.RData"))


# custom plotting theme
theme_clean <- function() {
  theme_classic(base_size = 16) 
}


# Global plot settings

pointsize = 3
linesize = 1
point_col = "black"
littoral = "green"
pelagic = "blue"
ribbon_col = "darkgrey"

# Explore the relationships =================

# global grand mean - average predicted outcome ignoring group deviations
# aka average marginal effects

# names(brm_mods_2015)
# names(reg_mod_data_2015)

# Hypothesis 1 - Asymmetric TP-body size relationship --------------------------

# plot function
plot_mass_x_alpha <- function(brm_mod_obj, mod_data){
  # set initial conditions for predictions
  int_conditions <- list(
    Alpha_mode = setNames(c(.2, .8), c("Neashore", "Pelagic"))
  )
  # get base plot of conditional effects
  p <- conditional_effects(brm_mod_obj, "log_mass:Alpha_mode", 
                           int_conditions = int_conditions, re_formula = NULL) 
  # custom plot
  p1 <- p$`log_mass:Alpha_mode` |> 
    ggplot(aes(x = exp(log_mass), y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = factor(Alpha_mode)), alpha = .2, show.legend = FALSE) +
    geom_line(aes(color = factor(Alpha_mode)), size = linesize, show.legend = FALSE) + 
    scale_color_manual(values=c(littoral, pelagic)) +
    scale_fill_manual(values=c(littoral, pelagic)) +
    ggnewscale::new_scale_fill() +
    geom_point(
      data = mod_data, aes(x = mass_g, y = TP_mode, fill = Alpha_mode),
      size = pointsize, shape = 21, color = point_col) +
    scale_fill_gradient(low = littoral, high = pelagic) +
    scale_x_log10(labels = scales::label_comma()) +
    coord_cartesian(ylim = c(1, max(mod_data$TP_mode) + 0.5)) +
    labs(title = "Asymmetric TP-body size relationship", 
         x = "Body mass (g)", y = "Trophic position", fill = "Alpha") + 
    theme_clean() + 
    theme(legend.position = c(0.08, .85))
}

# test it
# plot_mass_x_alpha(brm_mods_2015[["scale01"]][[1]], reg_mod_data_2015[["scale01"]])

# list mods and data
hyp1_mods <- list(
  brm_mods_2015[["scale01"]][[1]], 
  brm_mods_2015[["scale02a"]][[1]], 
  brm_mods_2015[["scale03a"]][[1]]
)

plot_data <- list(
  reg_mod_data_2015[["scale01"]], 
  reg_mod_data_2015[["scale02a"]], 
  reg_mod_data_2015[["scale03a"]]
)

# make plots for each scale
plots <- map2(hyp1_mods, plot_data, plot_mass_x_alpha)
names(plots) <- c("hyp1_scl01", "hyp1_scl02", "hyp1_scl03")
paths <- stringr::str_c(names(plots), ".png")
pwalk(list(paths, plots), ggsave, 
      path = here("out", "plots"), height = 6, width = 7, 
      device = png)



# Hypothesis 2 - Coupling of energy pathways ------------------------------

plot_mods_p2 <- list(
  brm_mods_2015[["scale01"]][[4]], 
  brm_mods_2015[["scale02a"]][[6]], 
  brm_mods_2015[["scale03a"]][[6]]
)

plot_p2 <- function(mods, data) {
  p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
  # plot(p, points = TRUE)
  p2 <- p$Alpha_mode |> 
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_line(size = linesize) + 
    geom_point(
      data = data, aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    scale_fill_gradient(low = littoral, high = pelagic) +
    coord_cartesian(ylim = c(min(data$TP_mode), max(data$TP_mode)+.5)) +
    labs(title = "Coupling - TP (spp. level)", 
         x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
    theme_clean() + 
    theme(legend.position = c(0.1, .85))
}
plots_p2 <- map2(plot_mods_p2,  plot_data, plot_p2)


plot_mods_p3 <- list(
  brm_mods_2015[["scale01_ind"]][[1]], 
  brm_mods_2015[["scale02a_ind"]][[1]], 
  brm_mods_2015[["scale03a_ind"]][[1]]
)

plot_data_ind <- list(
  reg_mod_data_2015[["scascale01_ind"]], 
  reg_mod_data_2015[["scascale02a_ind"]], 
  reg_mod_data_2015[["scascale03a_ind"]]
)

plot_p3 <- function(mods, data) {
  p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
  p3 <- p$Alpha_mode |> 
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_point(
      data = data, aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    geom_line(size = linesize) + 
    scale_fill_gradient(low = littoral, high = pelagic) +
    coord_cartesian(ylim = c(min(data$TP_mode), max(data$TP_mode)+.5)) +
    labs(title = "Coupling - TP (ind. level)", 
         x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
    theme_clean() + 
    theme(legend.position = "none")
}
plots_p3 <- map2(plot_mods_p3,  plot_data_ind, plot_p3)


plot_mods_p4 <- list(
  brm_mods_2015[["scale01"]][[7]], 
  brm_mods_2015[["scale02a"]][[11]], 
  brm_mods_2015[["scale03a"]][[11]]
)

plot_p4 <- function(mods, data) {
  p <- conditional_effects(mods, "Alpha_mode",  re_formula = NULL) 
  p4 <- p$Alpha_mode |> 
    mutate(across(c(estimate__, lower__, upper__), exp)) |>
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_point(
      data = data, aes(x = Alpha_mode, y = mass_g, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    geom_line(size = linesize) + 
    scale_fill_gradient(low = littoral, high = pelagic) +
    scale_y_log10(labels = scales::label_comma()) +
    labs(title = "Coupling - size (spp. level)", 
         x = "Alpha", y = "Body mass (g)") + 
    theme_clean() + 
    theme(legend.position = "none")
}
plots_p4 <- map2(plot_mods_p4,  plot_data, plot_p4)


# Compose figure

panel_01 <- 
  (plots_p2[[1]] | plots_p3[[1]] | plots_p4[[1]]) + 
  plot_annotation(tag_levels = 'A')

panel_02 <- 
  (plots_p2[[2]] | plots_p3[[2]] | plots_p4[[2]]) + 
  plot_annotation(tag_levels = 'A')

panel_03 <- 
  (plots_p2[[3]] | plots_p3[[3]] | plots_p4[[3]]) + 
  plot_annotation(tag_levels = 'A')

panel_plots <- list(panel_01, panel_02, panel_03)
names(panel_plots) <- c("hyp2_scl01", "hyp2_scl02", "hyp2_scl03")
paths <- stringr::str_c(names(panel_plots), ".png")
pwalk(list(paths, panel_plots), ggsave, 
      path = here("out", "plots"), height = 6, width = 18, 
      device = png)


