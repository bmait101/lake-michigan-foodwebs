
# Prep =========================================================================

# libraries
# source(here::here("R", "00_prep.R"))
library(here)        # file paths
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(tidybayes)
library(broom.mixed)
library(patchwork)

## Global plot settings ----
theme_clean <- function() {
  theme_classic(base_size = 8) 
}

box_line_size = .175
pointsize = .8
linesize = .8
point_col = "black"
littoral = "green"
pelagic = "blue"
ribbon_col = "darkgrey"


## Data ----
load(file = here("out", "data", "reg_mod_data_v3.RData"))

# list of plot data
plot_data_list <- list(
  reg_mod_data_tidy[["p1"]], 
  reg_mod_data_tidy[["s2"]], 
  reg_mod_data_tidy[["p4"]], 
  reg_mod_data_tidy[["s4"]],
  reg_mod_data_tidy[["p5"]]
)

# list of plot data for individual models
plot_data_list_ind <- list(
  reg_mod_data_tidy[["p1_id"]], 
  reg_mod_data_tidy[["s2_id"]], 
  reg_mod_data_tidy[["p4_id"]], 
  reg_mod_data_tidy[["s4_id"]],
  reg_mod_data_tidy[["p5_id"]]
)


## Model objects ----
load(file = here("out", "models", "brms", "brm_mods_list_20240328.RData"))


# Calcuate verticies ==================================================
# random samples from posterior then calculate vertex

# List of models to get x_vert
m_list_verts <- list(
  brm_mods_list[["brm_mods_1"]][[7]],
  brm_mods_list[["brm_mods_1"]][[4]],
  brm_mods_list[["brm_mods_1_ind"]][[1]],
  
  brm_mods_list[["brm_mods_2b_s2"]][[11]],
  brm_mods_list[["brm_mods_2b_s2"]][[6]],
  brm_mods_list[["brm_mods_2b_s2_ind"]][[1]],
  brm_mods_list[["brm_mods_2b_p4"]][[11]],
  brm_mods_list[["brm_mods_2b_p4"]][[6]],
  brm_mods_list[["brm_mods_2b_p4_ind"]][[1]],
  
  brm_mods_list[["brm_mods_3b_s4"]][[11]],
  brm_mods_list[["brm_mods_3b_s4"]][[6]],
  brm_mods_list[["brm_mods_3b_s4_ind"]][[1]],
  brm_mods_list[["brm_mods_3b_p5"]][[11]],
  brm_mods_list[["brm_mods_3b_p5"]][[8]],
  brm_mods_list[["brm_mods_3b_p5_ind"]][[1]]
)

# Name plots in list
names(m_list_verts) <- c(
  "1_7",
  "1_4",
  "1_ind",
  
  "2b_s2_11",
  "2b_s2_6",
  "2b_s2_ind",
  
  "2b_p4_11",
  "2b_p4_6",
  "2b_p4_ind",
  
  "3b_s4_11", 
  "3b_s4_6", 
  "3b_s4_ind", 
  
  "3b_p5_11",
  "3b_p5_8",
  "3b_p5_ind"
)

# testing

model <- m_list_verts[[1]]
model |> 
  spread_draws(`b_.*`, regex = TRUE) |> 
  slice_sample(n = 1000, replace = TRUE) |> 
  mutate(x_vert = (-b_Alpha_mode)/(2*b_IAlpha_modeE2)) |> 
  select(x_vert) 


# Function to get 1000 random alpha values at vertex from posterior and calcuate vertex
get_x_verts <- function(model) {
  model |> 
    spread_draws(`b_.*`, regex = TRUE) |> 
    slice_sample(n = 1000, replace = TRUE) |> 
    mutate(Alpha_mode = (-b_Alpha_mode)/(2*b_IAlpha_modeE2)) |> 
    select(Alpha_mode) 
}

# Get x_verts
verts <- m_list_verts |> 
  map(get_x_verts) |> 
  list_rbind(names_to = "model")

# Plot vertices
verts |> 
  ggplot(aes(y = Alpha_mode, x = model)) +
  coord_flip() + 
  geom_boxplot(outliers = FALSE) + 
  geom_hline(yintercept = c(0,1), linetype = "dashed") 

# Save verticies
# save(verts, file = here("out", "data", "verts.RData"))
load(file = here("out", "data", "verts.RData"))

# Summarize vertex samples by model group
verts |> 
  group_by(model) |> 
  summarize(
    mean = mean(Alpha_mode),
    median = median(Alpha_mode),
    sd = sd(Alpha_mode),
    lower = quantile(Alpha_mode, .025),
    upper = quantile(Alpha_mode, .975)
  ) |> 
  slice(10:15)


# Asymmetric TP-body size relationship =================================
## Quick plots ------
# int_conditions <- list(Alpha_mode = setNames(c(.2, .8), c("Neashore", "Pelagic")))
# plot(conditional_effects(brm_mods_list[["brm_mods_1"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions), points=TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_s2"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_p4"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_s4"]][[1]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_p5"]][[2]],"TP_mode:Alpha_mode", int_conditions = int_conditions, re_formula = NULL), points=TRUE)


## Custom plots ---------

# plot function
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
    # theme(legend.position = "none")
    theme(legend.position = c(0.2, .85), 
          legend.key.width = unit(.1, "cm"), 
          legend.key.height = unit(.2, "cm"))
  p1
}

# test it
plot_tp_x_alpha(brm_mods_list[["brm_mods_1"]][[1]], reg_mod_data_tidy[["p1"]])


## Loop over models/data and plot
plot_mods_asym <- list(
  brm_mods_list[["brm_mods_1"]][[2]], 
  brm_mods_list[["brm_mods_2b_s2"]][[2]], 
  brm_mods_list[["brm_mods_2b_p4"]][[1]],
  brm_mods_list[["brm_mods_3b_s4"]][[1]],
  brm_mods_list[["brm_mods_3b_p5"]][[2]]
)


# Make plots
plots_asym <- map2(plot_mods_asym, plot_data_list, plot_tp_x_alpha)

# Name plots in list
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



# TP ~ Alpha (species) ========================================================
### Quick plots ----------
# plot(conditional_effects(brm_mods_list[["brm_mods_1"]][[4]]), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_p4"]][[6]], re_formula = NA), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_s2"]][[6]], re_formula = NA), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_s4"]][[6]], re_formula = NA), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_p5"]][[8]], re_formula = NA), points = TRUE)

### Custom plots --------------

# plotting function
plot_alpha_tp <- function(mods, data, vert_data) {
  vert_data <- vert_data |> 
    mutate(estimate__ = max(data$TP_mode) + .75)
  
  p <- conditional_effects(mods, re_formula = NULL) 
  
  p2 <- p$Alpha_mode |> 
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_line(linewidth = linesize) + 
    geom_point(
      data = data, aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    geom_boxplot(
      data = vert_data, 
      aes(x = Alpha_mode, y = estimate__),
      outliers = FALSE, 
      width = .3, size = box_line_size) + 
    scale_fill_gradient(low = littoral, high = pelagic) +
    scale_y_continuous(limits = c(0.8, max(data$TP_mode) + 1), breaks = seq(1,5,1)) +
    # coord_cartesian(ylim = c(0.8, max(data$TP_mode)+ 1)) +
    labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
    theme_clean() + 
    # theme(legend.position = c(0.1, .85))+ 
    theme(legend.position = "none")
  p2
}

# Vert data for plots
vert_plot_dat <- verts |> 
  filter(model %in% c("1_4", "2b_s2_6", "2b_p4_6", "3b_s4_6", "3b_p5_8")) |> 
  mutate(model = factor(model, levels = c("1_4", "2b_s2_6", "2b_p4_6", "3b_s4_6", "3b_p5_8"))) |> 
  group_split(model)

# test it
# plot_alpha_tp(brm_mods_list[["brm_mods_1"]][[4]],  reg_mod_data_tidy[["p1"]])
# plot_alpha_tp(brm_mods_list[["brm_mods_3b_p5"]][[8]],  reg_mod_data_tidy[["p5"]])
# plot_alpha_tp(brm_mods_list[["brm_mods_1"]][[4]],  reg_mod_data_tidy[["p1"]], vert_plot_dat[[1]])


## list of models to plot
plot_mods_alpha_tp <- list(
  brm_mods_list[["brm_mods_1"]][[4]], 
  brm_mods_list[["brm_mods_2b_s2"]][[6]], 
  brm_mods_list[["brm_mods_2b_p4"]][[6]], 
  brm_mods_list[["brm_mods_3b_s4"]][[6]],
  brm_mods_list[["brm_mods_3b_p5"]][[8]]
)

# Make plots
# plots_alpha_tp <- map2(plot_mods_alpha_tp, plot_data_list, plot_alpha_tp)
plots_alpha_tp <- pmap(list(plot_mods_alpha_tp, plot_data_list, vert_plot_dat), plot_alpha_tp)

# Name plots in list
names(plots_alpha_tp) <- c(
  "alpha_tp_1_pooled", 
  "alpha_tp_2b_s2_region", 
  "alpha_tp_2b_p4_region", 
  "alpha_tp_3b_s4_regionXseason",
  "alpha_tp_3b_p5_regionXseason"
)


# check it
plots_alpha_tp[[1]]
plots_alpha_tp[[2]]
plots_alpha_tp[[3]]
plots_alpha_tp[[4]]
plots_alpha_tp[[5]]



# TP ~ alpha (ind ========================================================

### Quick plots -----------
# plot(conditional_effects(brm_mods_list[["brm_mods_1_ind"]][[1]]), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_s2_ind"]][[1]], re_formula = NULL), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_p4_ind"]][[1]], re_formula = NULL), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_s4_ind"]][[1]], re_formula = NULL), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_p5_ind"]][[1]], re_formula = NULL), points = TRUE)

### Custom plots --------------

# plotting function
plot_alpha_tp_ind <- function(mods, data, vert_data) {
  vert_data <- vert_data |> 
    mutate(estimate__ = max(data$TP_mode) + .75)
  p <- conditional_effects(mods, "Alpha_mode", re_formula = NA) 
  p2 <- p$Alpha_mode |> 
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_point(
      data = data, aes(x = Alpha_mode, y = TP_mode, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    geom_line(linewidth = linesize) + 
    geom_boxplot(
      data = vert_data, 
      aes(x = Alpha_mode, y = estimate__),
      outliers = FALSE, 
      width = 0.3, size = box_line_size) +     
    scale_fill_gradient(low = littoral, high = pelagic) +
    # scale_y_continuous(breaks = seq(-20,10,1))+
    scale_y_continuous(breaks = seq(1,5,1)) +
    # scale_x_continuous(breaks = seq(0,1,.25)) +
    coord_cartesian(
      xlim = c(0, 1),
      ylim = c(1, max(data$TP_mode)+1)
      ) +
    # coord_cartesian(ylim = c(min(data$TP_mode), max(data$TP_mode)+1), clip = "off") +
    labs(x = "Alpha", y = "Trophic Position", fill = "Alpha") + 
    theme_clean() + 
    # theme(legend.position = c(0.1, .85))+ 
    theme(legend.position = "none")
  p2
}

# Vert data for plots
vert_plot_dat <- verts |> 
  filter(model %in% c("1_ind", "2b_s2_ind", "2b_p4_ind", "3b_s4_ind", "3b_p5_ind")) |> 
  mutate(model = factor(model, levels = c("1_ind", "2b_s2_ind", "2b_p4_ind", "3b_s4_ind", "3b_p5_ind"))) |> 
  group_split(model)

# test it
# plot_alpha_tp_ind(brm_mods_list[["brm_mods_1_ind"]][[1]], reg_mod_data_tidy[["p1_id"]], vert_plot_dat[[1]])
# plot_alpha_tp_ind(brm_mods_list[["brm_mods_3b_s4_ind"]][[1]], reg_mod_data_tidy[["s4_id"]], vert_plot_dat[[4]])



## Loop over models/data and plot
plot_mods_alpha_tp_ind <- list(
  brm_mods_list[["brm_mods_1_ind"]][[1]], 
  brm_mods_list[["brm_mods_2b_s2_ind"]][[1]], 
  brm_mods_list[["brm_mods_2b_p4_ind"]][[1]], 
  brm_mods_list[["brm_mods_3b_s4_ind"]][[1]],
  brm_mods_list[["brm_mods_3b_p5_ind"]][[1]]
)


# Make plots
plots_alpha_tp_ind <- pmap(list(plot_mods_alpha_tp_ind, plot_data_list_ind, vert_plot_dat), plot_alpha_tp_ind)
# plots_alpha_tp_ind <- map2(plot_mods_alpha_tp_ind, plot_data_list_ind, plot_alpha_tp_ind)

# Name plots in list
names(plots_alpha_tp_ind) <- c(
  "alpha_maass_ind_1_pooled", 
  "alpha_maass_ind_2b_s2_region", 
  "alpha_maass_ind_2b_p4_region", 
  "alpha_maass_ind_3b_s4_regionXseason",
  "alpha_maass_ind_3b_p5_regionXseason"
)

# Check it
plots_alpha_tp_ind[[3]]

# Mass ~ alpha ========================================================

### Quick plots -----------------
# plot(conditional_effects(brm_mods_list[["brm_mods_1"]][[7]]), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_p4"]][[11]], re_formula = NULL), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_2b_s2"]][[11]], re_formula = NULL), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_p5"]][[11]], re_formula = NULL), points = TRUE)
# plot(conditional_effects(brm_mods_list[["brm_mods_3b_s4"]][[11]], re_formula = NULL), points = TRUE)

### Custom plots ----------------

# plot function
plot_alpha_mass <- function(mods, data, vert_data) {
  vert_data <- vert_data |> 
    mutate(estimate__ = max(data$log_mass) + 2)
  
  p <- conditional_effects(mods, "Alpha_mode",  re_formula = NULL) 
  
  p4 <- p$Alpha_mode |> 
    # mutate(across(c(estimate__, lower__, upper__), exp)) |>
    ggplot(aes(x = Alpha_mode, y = estimate__)) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_col, alpha = .5) +
    geom_point(
      data = data, aes(x = Alpha_mode, y = log_mass, fill = Alpha_mode),
      size = pointsize, color = point_col, shape = 21) +
    geom_boxplot(
      data = vert_data, 
      aes(x = Alpha_mode, y = estimate__),
      outliers = FALSE, 
      width = 1, size = box_line_size) +   
    geom_line(linewidth = linesize) + 
    scale_fill_gradient(low = littoral, high = pelagic) +
    # coord_cartesian(ylim = c(min(data$log_mass)-1, max(data$log_mass)+1)) +
    # scale_y_continuous(breaks = seq(-20,20,5))+
    # scale_y_log10(labels = scales::label_log()) +
    labs(x = "Alpha", y = "log body mass (g)") + 
    theme_clean() + 
    theme(legend.position = "none")
  p4
}

# test it
plot_alpha_mass(brm_mods_list[["brm_mods_1"]][[7]],  reg_mod_data_tidy[["p1"]], vert_plot_dat[[1]])
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

# Vert data for plots
vert_plot_dat <- verts |> 
  filter(model %in% c("1_7", "2b_s2_11", "2b_p4_11", "3b_s4_11", "3b_p5_11")) |> 
  mutate(model = factor(model, levels = c("1_7", "2b_s2_11", "2b_p4_11", "3b_s4_11", "3b_p5_11"))) |> 
  group_split(model)

# Make plots
plots_alpha_mass <- pmap(list(plot_mods_alpha_mass, plot_data_list, vert_plot_dat), plot_alpha_mass)

# Name plots
names(plots_alpha_mass) <- c(
  "alpha_mass_1_pooled", 
  "alpha_mass_2b_s2_region", 
  "alpha_mass_2b_p4_region", 
  "alpha_mass_3b_s4_regionXseason",
  "alpha_mass_3b_p5_regionXseason"
)

# Check
plots_alpha_mass[[5]]

# Compose figure ========================================================

# names(plots_asym)
# names(plots_alpha_tp)
# names(plots_alpha_tp_ind)
# names(plots_alpha_mass)

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


# List plots
panel_plots <- list(p1, p2, p3, p4, p5)

# Name plots
names(panel_plots) <- c(
  "results_1_pooled_r1",
  "results_2b_s2_region_r1",
  "results_2b_p4_region_r1",
  "results_3b_s4_regionXseason_r1",
  "results_3b_p5_regionXseason_r1"
)

# Save to file
paths <- stringr::str_c(names(panel_plots), ".png")
pwalk(
  list(paths, panel_plots), ggsave,
  path = here("out", "plots", "R1"),
  height = 5,
  width = 18,
  units = "cm",
  device = png
)




p4 / p5



# decrease legend size 
p <- (plots_asym[[4]])
p <- p + theme(legend.title = element_text(size = 6), 
               legend.text = element_text(size = 6))

p11 <- ( 
  (p | plots_alpha_mass[[4]] | plots_alpha_tp[[4]] | plots_alpha_tp_ind[[4]]) /
    (plots_asym[[5]]+theme(legend.position = "none") | plots_alpha_mass[[5]] | plots_alpha_tp[[5]] | plots_alpha_tp_ind[[5]]) 
) + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

ggsave(
  here("out","plots","R1","result_3ab_wide_r1_box.png"),
  p11, 
  height = 9, 
  width = 18, 
  units = "cm", 
  device = png
)

path <- here::here("out","plots","R1","result_3ab_wide_final_r1_box")
ggsave(glue::glue("{path}.pdf"), plot = p11, 
       width = 18, height = 9, units = "cm", scale = 1, device = cairo_pdf)
pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 300)


# END
