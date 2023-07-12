
remotes::install_github("ropensci/rfishbase")
library(rfishbase)

# list of fish species
fish_df <- reg_mod_data_tidy[[1]] |> select(species)
fish_df <- fish_df |> 
  left_join(xref_sci_names, by = c("species" = "common_name")) |> 
  filter(compartment == "fishes")

# get tps for each fish species
fishbase_TPs <- list()
for (i in 1:length(fish_df$sci_name)){
  fishbase_TPs[i] <- ecology(fish_df$sci_name[i], fields = c("DietTroph"))
}

# make field for fishbase tps
fish_df <- mutate(fish_df, tp_FB = NA_real_)

# add fishbase tps to df
for (i in 1:length(fish_df$tp_FB)){
  fish_df$tp_FB[i] <- fishbase_TPs[[i]]
}
fish_df

# add missing TP values 
(fish_df <- fish_df |> 
  mutate(tp_FB = case_when(
    species=="slimy sculpin" ~ 3.37, 
    species=="deepwater sculpin" ~ 3.24, 
    species=="coho salmon" ~ 4.18, 
    TRUE ~ tp_FB)
    ))


# TEST - merge back with model data and plot for one scale
fish_plot <- reg_mod_data_tidy[[2]] |> 
  left_join(xref_sci_names, by = c("species" = "common_name")) |> 
  filter(compartment == "fishes") |> 
  # select(species, TP_mode) |> 
  left_join(fish_df |> select(species, tp_FB), by = "species")
fish_plot

fish_plot |> 
  ggplot(aes(tp_FB, TP_mode)) + 
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50,  label.size = 0.25, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") + 
  scale_x_continuous(limits = c(2, 4.5)) +
  scale_y_continuous(limits = c(2, 4.5)) +
  labs(x = expression(TP[FishBase]), y = expression(TP[{delta}^15*N~'(\u2030)'])) + 
  theme_clean()

# Loop over regression model datasets (scales) and add fishbase TPs
add_fb_tps <- function(dataset) {
  fish_plot <- dataset |> 
    left_join(xref_sci_names, by = c("species" = "common_name")) |> 
    filter(compartment == "fishes") |> 
    # select(species, TP_mode) |> 
    left_join(fish_df |> select(species, tp_FB), by = "species")
  fish_plot
}
fishTP_plot_data <- reg_mod_data_tidy[c(1:9)] |> map(add_fb_tps)
fishTP_plot_data$s2

fishTP_plot_data[[1]] |> 
  ggplot(aes(tp_FB, TP_mode)) + 
  geom_point(aes(color = species)) + 
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 10000, size=2) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") + 
  scale_x_continuous(limits = c(2, 5)) +
  scale_y_continuous(limits = c(2, 5)) +
  scale_color_manual(values = pals::cols25()) +
  labs(x = expression(TP[FishBase]), y = expression(TP[{delta}^15*N~'(\u2030)'])) + 
  theme_clean()

# Plots ----------------------------------


make_tp_tp_plot <- function(data) {
  p <- data |> 
    ggplot(aes(tp_FB, TP_mode)) + 
    geom_point(aes(color = species)) + 
    ggrepel::geom_text_repel(aes(label = species), max.overlaps = 10000, size=2) +
    geom_abline(intercept = 0, slope = 1, linetype="dashed") + 
    scale_x_continuous(limits = c(2, 5)) +
    scale_y_continuous(limits = c(2, 5)) +
    scale_color_manual(values = pals::cols25()) +
    labs(x = expression(TP[FishBase]), y = expression(TP[{delta}^15*N~'(\u2030)'])) + 
    theme_clean()
  p
}

make_tp_tp_plot(fishTP_plot_data$p4)

fish_tp_tp_plots <- list()
for (i in 1:length(fishTP_plot_data)) {
  fish_tp_tp_plots[[i]] <- make_tp_tp_plot(fishTP_plot_data[[i]])
}

# check
names(fish_tp_tp_plots) <- c(
  "tp_tp_scale01_p1", 
  "tp_tp_scale02a_p2", 
  "tp_tp_scale03a_p3", 
  "tp_tp_scale02b_p4", 
  "tp_tp_scale03b_p5",
  "tp_tp_scale02a_s1", 
  "tp_tp_scale02b_s2",
  "tp_tp_scale03a_s3", 
  "tp_tp_scale03b_s4"
)
paths <- stringr::str_c(names(fish_tp_tp_plots), ".png")
pwalk(list(paths, fish_tp_tp_plots), ggsave, 
      path = here("out", "plots"), height = 5, width = 8, 
      device = png)





# Correlations --------------------------------



fish_na <- drop_na(fish, tp_FB)
cor(fish_na$tp_FB, fish_na$TP_mode, method = "pearson")
