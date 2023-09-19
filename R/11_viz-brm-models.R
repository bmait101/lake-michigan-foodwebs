
source(here::here("R", "00_prep.R"))

# Data

load(file = here("out", "data", "reg_mod_data_v3.RData"))

# Model objects

load(file = here("out", "models", "brms", "brm_mods_list.RData"))
load(file = here("out", "models", "brms", "brm_mods_list_asym.RData"))

names(brm_mods_list)
names(brm_mods_list_asym)
names(reg_mod_data_tidy)

# Global plot settings
theme_clean <- function() {
  theme_classic(base_size = 8) 
}
pointsize = 1
linesize = .8
point_col = "black"
littoral = "green"
pelagic = "blue"
ribbon_col = "darkgrey"

# Explore the relationships =================
# global grand mean - average predicted outcome ignoring group deviations
# aka average marginal effects


# Asymmetric TP-body size relationship --------------------------
## Quick plots ------
int_conditions <- list(Alpha_mode = setNames(c(.2, .8), c("Neashore", "Pelagic")))

# Scale 1
# plot(conditional_effects(brm_mods_list_asym[["brm_mods_1_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions), points=TRUE)
plot(conditional_effects(brm_mods_list_asym[["brm_mods_1_asym"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions), points=TRUE)

# Scale 2 - specific
# plot(conditional_effects(brm_mods_list_asym[["brm_mods_2b_s2_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)
plot(conditional_effects(brm_mods_list_asym[["brm_mods_2b_s2_asym"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)

# Scale 2 - pooled
plot(conditional_effects(brm_mods_list_asym[["brm_mods_2b_p4_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)
# plot(conditional_effects(brm_mods_list_asym[["brm_mods_2b_p4_asym"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)

# Scale 3 - specific
plot(conditional_effects(brm_mods_list_asym[["brm_mods_3b_s4_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)
# plot(conditional_effects(brm_mods_list_asym[["brm_mods_3b_s4_asym"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)

# Scale 3 - pooled
# plot(conditional_effects(brm_mods_list_asym[["brm_mods_3b_p5_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)
plot(conditional_effects(brm_mods_list_asym[["brm_mods_3b_p5_asym"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)


# conditional_effects(brm_mods_list[["brm_mods_2a_p2"]][[1]],"log_mass:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)
# conditional_effects(brm_mods_list_asym[["brm_mods_2a_p2_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)
# conditional_effects(brm_mods_list[["brm_mods_2a_s1"]][[1]],"log_mass:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)
# conditional_effects(brm_mods_list_asym[["brm_mods_2a_s1_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)
# conditional_effects(brm_mods_list[["brm_mods_3a_p3"]][[1]],"log_mass:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)
# conditional_effects(brm_mods_list_asym[["brm_mods_3a_p3_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)
# conditional_effects(brm_mods_list[["brm_mods_3a_s3"]][[1]],"log_mass:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)
# conditional_effects(brm_mods_list_asym[["brm_mods_3a_s3_asym"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL)


## Custom plots ---------

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
    geom_line(aes(color = factor(Alpha_mode)), linewidth = linesize, show.legend = FALSE) + 
    scale_color_manual(values=c(littoral, pelagic)) +
    scale_fill_manual(values=c(littoral, pelagic)) +
    ggnewscale::new_scale_fill() +
    geom_point(
      data = mod_data, aes(x = mass_g, y = TP_mode, fill = Alpha_mode),
      size = pointsize, shape = 21, color = point_col) +
    scale_fill_gradient(low = littoral, high = pelagic) +
    scale_x_log10(labels = scales::label_log()) +
    scale_y_continuous(breaks = seq(0,5,1)) +
    coord_cartesian(ylim = c(0, max(mod_data$TP_mode) + 0.5)) +
    labs(x = "Body mass (g)", y = "Trophic position", fill = "Alpha") + 
    theme_clean() + 
    theme(legend.position = c(0.08, .85))
  p1
}
plot_tp_x_alpha <- function(brm_mod_obj, mod_data){
  # set initial conditions for predictions
  int_conditions <- list(
    Alpha_mode = setNames(c(.2, .8), c("Neashore", "Pelagic"))
  )
  # get base plot of conditional effects
  p <- conditional_effects(brm_mod_obj, "TP_mode:Alpha_mode", 
                           int_conditions = int_conditions, re_formula = NULL) 
  # custom plot
  p1 <- p$`TP_mode:Alpha_mode` |> 
    # mutate(across(c(estimate__, lower__, upper__), exp)) |>
    ggplot(aes(x = TP_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = factor(Alpha_mode)), alpha = .2, show.legend = FALSE) +
    geom_line(aes(color = factor(Alpha_mode)), linewidth = linesize, show.legend = FALSE) + 
    scale_color_manual(values=c(littoral, pelagic)) +
    scale_fill_manual(values=c(littoral, pelagic)) +
    ggnewscale::new_scale_fill() +
    geom_point(data = mod_data, aes(x = TP_mode, y = log_mass, fill = Alpha_mode),size = pointsize, shape = 21, color = point_col) +
    scale_fill_gradient(low = littoral, high = pelagic) +
    # scale_y_log10(labels = scales::label_log()) +
    # coord_cartesian(xlim = c(0, max(mod_data$TP_mode) + 0.5)) +
    labs(y = "log body mass (g)", x = "Trophic position", fill = "Alpha") + 
    theme_clean() + 
    # theme(legend.position = c(0.08, .85)) + 
    theme(legend.position = "none")
  p1
}

# test it
plot_mass_x_alpha(brm_mods_list[["brm_mods_1"]][[1]], reg_mod_data_tidy[["p1"]])
# plot_tp_x_alpha(brm_mods_list_asym[["brm_mods_1_asym"]][[1]], reg_mod_data_tidy[["p1"]])


## Loop over models/data and plot
plot_mods_asym <- list(
  brm_mods_list_asym[["brm_mods_1_asym"]][[2]], 
  brm_mods_list_asym[["brm_mods_2b_s2_asym"]][[2]], 
  brm_mods_list_asym[["brm_mods_2b_p4_asym"]][[1]],
  brm_mods_list_asym[["brm_mods_3b_s4_asym"]][[1]],
  brm_mods_list_asym[["brm_mods_3b_p5_asym"]][[2]]
)

plot_data_list <- list(
  reg_mod_data_tidy[["p1"]], 
  reg_mod_data_tidy[["s2"]], 
  reg_mod_data_tidy[["p4"]], 
  reg_mod_data_tidy[["s4"]],
  reg_mod_data_tidy[["p5"]]
)

plots_asym <- map2(plot_mods_asym, plot_data_list, plot_tp_x_alpha)
names(plots_asym) <- c(
  "asym_1_pooled", 
  "asym_2b_s2_region", 
  "asym_2b_p4_region", 
  "asym_3b_s4_regionXseason",
  "asym_3b_p5_regionXseason"
)

# Write to file
# paths <- stringr::str_c(names(plots_asym), ".png")
# pwalk(list(paths, plots_asym), ggsave, 
#       path = here("out", "plots"), height = 6, width = 7, 
#       device = png)



# Coupling of energy pathways ------------------------------

## Alpha ~ TP (species) ---------------------------------------

### Quick plots ----------
plot(conditional_effects(brm_mods_list[["brm_mods_1"]][[4]]), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_2b_p4"]][[6]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_2b_s2"]][[6]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_3b_p5"]][[6]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_3b_s4"]][[8]], re_formula = NULL), points = TRUE)

### Custom plots --------------

# plotting function
plot_alpha_tp <- function(mods, data) {
  p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
  # plot(p, points = TRUE)
  p2 <- p$Alpha_mode |> 
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_line(linewidth = linesize) + 
    geom_point(
      data = data, aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    scale_fill_gradient(low = littoral, high = pelagic) +
    coord_cartesian(ylim = c(min(data$TP_mode), max(data$TP_mode)+.5)) +
    labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
    theme_clean() + 
    # theme(legend.position = c(0.1, .85))+ 
    theme(legend.position = "none")
  p2
}

# test it
plot_alpha_tp(brm_mods_list[["brm_mods_1"]][[4]],  reg_mod_data_tidy[["p1"]])
plot_alpha_tp(brm_mods_list[["brm_mods_2b_p4"]][[6]],  reg_mod_data_tidy[["p2"]])

## Loop over models/data and plot
plot_mods_alpha_tp <- list(
  brm_mods_list[["brm_mods_1"]][[4]], 
  brm_mods_list[["brm_mods_2b_s2"]][[6]], 
  brm_mods_list[["brm_mods_2b_p4"]][[6]], 
  brm_mods_list[["brm_mods_3b_s4"]][[6]],
  brm_mods_list[["brm_mods_3b_p5"]][[8]]
)

plots_alpha_tp <- map2(plot_mods_alpha_tp, plot_data_list, plot_alpha_tp)
names(plots_alpha_tp) <- c(
  "alpha_tp_1_pooled", 
  "alpha_tp_2b_s2_region", 
  "alpha_tp_2b_p4_region", 
  "alpha_tp_3b_s4_regionXseason",
  "alpha_tp_3b_p5_regionXseason"
)

## Alpha ~ TP (ind) ---------------------------------------

### Quick plots -----------
plot(conditional_effects(brm_mods_list[["brm_mods_1_ind"]][[1]]), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_2b_s2_ind"]][[1]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_2b_p4_ind"]][[1]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_3b_s4_ind"]][[1]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_3b_p5_ind"]][[1]], re_formula = NULL), points = TRUE)

### Custom plots --------------

# plotting function
plot_alpha_tp_ind <- function(mods, data) {
  p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
  # plot(p, points = TRUE)
  p2 <- p$Alpha_mode |> 
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_point(
      data = data, aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    geom_line(linewidth = linesize) + 
    scale_fill_gradient(low = littoral, high = pelagic) +
    coord_cartesian(ylim = c(min(data$TP_mode), max(data$TP_mode)+.5)) +
    labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
    theme_clean() + 
    # theme(legend.position = c(0.1, .85))+ 
    theme(legend.position = "none")
  p2
}

# test it
plot_alpha_tp_ind(brm_mods_list[["brm_mods_1_ind"]][[1]],  reg_mod_data_tidy[["p1_id"]])
plot_alpha_tp_ind(brm_mods_list[["brm_mods_2b_p4_ind"]][[1]],  reg_mod_data_tidy[["p4_id"]])
plot_alpha_tp_ind(brm_mods_list[["brm_mods_3b_p5_ind"]][[1]],  reg_mod_data_tidy[["p5_id"]])


## Loop over models/data and plot
plot_mods_alpha_tp_ind <- list(
  brm_mods_list[["brm_mods_1_ind"]][[1]], 
  brm_mods_list[["brm_mods_2b_s2_ind"]][[1]], 
  brm_mods_list[["brm_mods_2b_p4_ind"]][[1]], 
  brm_mods_list[["brm_mods_3b_s4_ind"]][[1]],
  brm_mods_list[["brm_mods_3b_p5_ind"]][[1]]
)

plot_data_list_ind <- list(
  reg_mod_data_tidy[["p1_id"]], 
  reg_mod_data_tidy[["s2_id"]], 
  reg_mod_data_tidy[["p4_id"]], 
  reg_mod_data_tidy[["s4_id"]],
  reg_mod_data_tidy[["p5_id"]]
)


plots_alpha_tp_ind <- map2(plot_mods_alpha_tp_ind, plot_data_list_ind, plot_alpha_tp_ind)
names(plots_alpha_tp_ind) <- c(
  "alpha_maass_ind_1_pooled", 
  "alpha_maass_ind_2b_s2_region", 
  "alpha_maass_ind_2b_p4_region", 
  "alpha_maass_ind_3b_s4_regionXseason",
  "alpha_maass_ind_3b_p5_regionXseason"
)


## Alpha ~ mass -------------------------------- 

### Quick plots -----------------
plot(conditional_effects(brm_mods_list[["brm_mods_1"]][[7]]), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_2b_p4"]][[11]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_2b_s2"]][[11]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_3b_p5"]][[11]], re_formula = NULL), points = TRUE)
plot(conditional_effects(brm_mods_list[["brm_mods_3b_s4"]][[11]], re_formula = NULL), points = TRUE)

### Custom plots ----------------

# plot function
plot_alpha_mass <- function(mods, data) {
  p <- conditional_effects(mods, "Alpha_mode",  re_formula = NULL) 
  p4 <- p$Alpha_mode |> 
    # mutate(across(c(estimate__, lower__, upper__), exp)) |>
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_point(
      data = data, aes(x = Alpha_mode, y = log_mass, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    geom_line(linewidth = linesize) + 
    scale_fill_gradient(low = littoral, high = pelagic) +
    scale_y_continuous(breaks = seq(-20,20,5))+
    # scale_y_log10(labels = scales::label_log()) +
    labs(x = "Alpha", y = "log body mass (g)") + 
    theme_clean() + 
    theme(legend.position = "none")
  p4
}

# test it
# plot_alpha_mass(brm_mods_list[["brm_mods_1"]][[7]],  reg_mod_data_tidy[["p1"]])
# plot_alpha_mass(brm_mods_list[["brm_mods_2a_s1"]][[11]],  reg_mod_data_tidy[["s1"]])

## Loop over models/data and plot
plot_mods_alpha_mass <- list(
  brm_mods_list[["brm_mods_1"]][[7]], 
  brm_mods_list[["brm_mods_2b_s2"]][[11]], 
  brm_mods_list[["brm_mods_2b_p4"]][[11]], 
  brm_mods_list[["brm_mods_3b_s4"]][[11]],
  brm_mods_list[["brm_mods_3b_p5"]][[11]]
)

plots_alpha_mass <- map2(plot_mods_alpha_mass, plot_data_list, plot_alpha_mass)
names(plots_alpha_mass) <- c(
  "alpha_mass_1_pooled", 
  "alpha_mass_2b_s2_region", 
  "alpha_mass_2b_p4_region", 
  "alpha_mass_3b_s4_regionXseason",
  "alpha_mass_3b_p5_regionXseason"
)


# Compose figure ---------------------------------------

names(plots_asym)
names(plots_alpha_tp)
names(plots_alpha_tp_ind)
names(plots_alpha_mass)


p1 <- ((plots_asym[[1]] | plots_alpha_mass[[1]] | plots_alpha_tp[[1]] | plots_alpha_tp_ind[[1]])) + 
  plot_annotation(
    title = "Scale 1: pooled data", 
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")"
    )

p2 <- ((plots_asym[[2]] | plots_alpha_mass[[2]] | plots_alpha_tp[[2]] | plots_alpha_tp_ind[[2]])) + 
  plot_annotation(
    title = "Scale 2 (by region): specific baselines", 
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")"
    )

p3 <- ((plots_asym[[3]] | plots_alpha_mass[[3]] | plots_alpha_tp[[3]] | plots_alpha_tp_ind[[3]])) + 
  plot_annotation(
    title = "Scale 2 (by region): pooled baseline", 
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")"
    )

p4 <- ((plots_asym[[4]] | plots_alpha_mass[[4]] | plots_alpha_tp[[4]] | plots_alpha_tp_ind[[4]])) + 
  plot_annotation(
    title = "Scale 3 (by region, season): specific baselines", 
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")"
    )

p5 <- ((plots_asym[[5]] | plots_alpha_mass[[5]] | plots_alpha_tp[[5]] | plots_alpha_tp_ind[[5]])) + 
  plot_annotation(
    title = "Scale 3 (by region, season): pooled baseline", 
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")"
    )


# List plots, name, and save to file
panel_plots <- list(p1, p2, p3, p4, p5)
names(panel_plots) <- c(
  "results_1_pooled", 
  "results_2b_s2_region", 
  "results_2b_p4_region", 
  "results_3b_s4_regionXseason",
  "results_3b_p5_regionXseason"
)

paths <- stringr::str_c(names(panel_plots), ".png")
pwalk(
  list(paths, panel_plots), ggsave, 
  path = here("out", "plots"),
  height = 5, 
  width = 18, 
  units = "cm", 
  device = png
  )
 

p11 <- ( 
  (plots_asym[[4]] | plots_alpha_mass[[4]] | plots_alpha_tp[[4]] | plots_alpha_tp_ind[[4]]) /
    (plots_asym[[5]] | plots_alpha_mass[[5]] | plots_alpha_tp[[5]] | plots_alpha_tp_ind[[5]]) 
) + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

ggsave(
  here("out","plots","result_3ab_wide"),
  p11, 
  height = 9, 
  width = 18, 
  units = "cm", 
  device = png
  )






 # EXTRA ============================

mods <- brm_mods_list[["brm_mods_3b_p5"]][[6]]
dat <- reg_mod_data_tidy[["p5"]]
p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
# plot(p, points = TRUE)
p12 <- p$Alpha_mode |> 
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_line(linewidth = linesize) + 
  geom_point(
    # data = dat |> unite("baseline", lake_region, season), aes(x = Alpha_mode, y = TP_mode, color = species, shape = baseline),
    data = dat, aes(x = Alpha_mode, y = TP_mode, color = season, shape = lake_region),
    size = 3) +
  # ggrepel::geom_text_repel(data = dat, aes(x = Alpha_mode, y = TP_mode, label = species), size = 3) + 
  facet_wrap(vars(species)) + 
  # scale_shape_manual(values = c(0,1,2,4,5,6,7,13,15,16,17,18)) + 
  # scale_fill_gradient(low = littoral, high = pelagic) +
  coord_cartesian(ylim = c(min(dat$TP_mode), max(dat$TP_mode)+.5)) +
  labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
  theme_classic(base_size = 16)

ggsave(here("out","plots","result_species_panel_p5"), p12, height = 30, width = 40, units = "cm", device = png)


mods <- brm_mods_list[["brm_mods_3b_s4"]][[6]]
dat <- reg_mod_data_tidy[["s4"]]
p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
# plot(p, points = TRUE)
p13 <- p$Alpha_mode |> 
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_line(linewidth = linesize) + 
  geom_point(
    # data = dat |> unite("baseline", lake_region, season), aes(x = Alpha_mode, y = TP_mode, color = species, shape = baseline),
    data = dat, aes(x = Alpha_mode, y = TP_mode, color = season, shape = lake_region),
    size = 3) +
  # ggrepel::geom_text_repel(data = dat, aes(x = Alpha_mode, y = TP_mode, label = species), size = 3) + 
  facet_wrap(vars(species)) + 
  # scale_shape_manual(values = c(0,1,2,4,5,6,7,13,15,16,17,18)) + 
  # scale_fill_gradient(low = littoral, high = pelagic) +
  coord_cartesian(ylim = c(min(dat$TP_mode), max(dat$TP_mode)+.5)) +
  labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
  theme_classic(base_size = 16)


ggsave(here("out","plots","result_species_panel_s4"), p13, height = 30, width = 40, units = "cm", device = png)



mods <- brm_mods_list[["brm_mods_3b_p5"]][[11]]
dat <- reg_mod_data_tidy[["p5"]]
p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
# plot(p, points = TRUE)
p14 <-p$Alpha_mode |> 
  # mutate(across(c(estimate__, lower__, upper__), exp)) |>
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_point(
    data = dat, aes(x = Alpha_mode, y = log_mass, color = season, shape = lake_region),
    size = 3) +
  geom_line(linewidth = linesize) + 
  facet_wrap(vars(species)) + 
  scale_fill_gradient(low = littoral, high = pelagic) +
  scale_y_continuous(breaks = seq(-20,20,5))+
  # scale_y_log10(labels = scales::label_log()) +
  labs(x = "Alpha", y = "log body mass (g)") + 
  theme_classic(base_size = 16)

ggsave(here("out","plots","result_species_panel_p5_wei"), p14, height = 30, width = 40, units = "cm", device = png)


mods <- brm_mods_list[["brm_mods_3b_s4"]][[11]]
dat <- reg_mod_data_tidy[["s4"]]
p <- conditional_effects(mods, "Alpha_mode", re_formula = NULL) 
# plot(p, points = TRUE)
p15 <- p$Alpha_mode |> 
  # mutate(across(c(estimate__, lower__, upper__), exp)) |>
  ggplot(aes(x = Alpha_mode, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
  geom_point(
    data = dat, aes(x = Alpha_mode, y = log_mass, color = season, shape = lake_region),
    size = 3) +
  geom_line(linewidth = linesize) + 
  facet_wrap(vars(species)) + 
  scale_fill_gradient(low = littoral, high = pelagic) +
  scale_y_continuous(breaks = seq(-20,20,5))+
  # scale_y_log10(labels = scales::label_log()) +
  labs(x = "Alpha", y = "log body mass (g)") + 
  theme_classic(base_size = 16)


ggsave(here("out","plots","result_species_panel_s4_wei"), p15, height = 30, width = 40, units = "cm", device = png)












nd.1 <- expand_grid(
  basin = levels(reg_mod_data_tidy[["s3"]]$basin)[[1]], 
  season = levels(reg_mod_data_tidy[["s3"]]$season)[[1]], 
  Alpha_mode = c(0.2,0.8), 
  TP_mode = seq(1,max(reg_mod_data_tidy[["s3"]]$TP_mode), length.out = 200)
)

pred <- brm_mods_list_asym[["brm_mods_3a_s3_asym"]][[1]] |>  
  epred_draws(newdata = nd.1) |>
  mutate(Alpha_mode = as.factor(Alpha_mode)) %>% 
  mutate(Alpha_mode = recode(Alpha_mode, "0.2" = "Littoral", "0.8" = "Pelagic")) |> 
  rename(log_mass = .epred)


pred |> 
  ggplot(aes(x = TP_mode, y = log_mass, group = Alpha_mode, color = Alpha_mode)) +
  stat_lineribbon(.width = c(0.8))  + 
  geom_point(data = reg_mod_data_tidy[["s3"]], aes(x = log_mass, y = TP_mode))


reg_mod_data_tidy[["p1"]] |> 
  # group_by(cyl) %>%
  modelr::data_grid(TP_mode = modelr::seq_range(TP_mode, n = 51), 
                    Alpha_mode = c(0.3,0.7)
  ) %>%
  add_epred_draws(brm_mods_list_asym[["brm_mods_1_asym"]][[1]]) %>%
  mutate(Alpha_mode = as.factor(Alpha_mode)) %>% 
  mutate(Alpha_mode = recode(Alpha_mode, "0.2" = "Littoral", "0.8" = "Pelagic")) |> 
  # filter(!(TP_mode <= 1.4 & Alpha_mode == "Pelagic")) |> 
  ggplot(aes(x = TP_mode, y = log_mass)) +
  stat_lineribbon(aes(y = .epred, color = Alpha_mode)) +
  geom_point(data = reg_mod_data_tidy[["p1"]]) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") + 
  theme_clean()

