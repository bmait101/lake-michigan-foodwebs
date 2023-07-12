
## PLot and summarize estimated TP and alpha data



source(here::here("R", "00_prep.R"))

names(brm_mods_list)
names(brm_mods_list_asym)
names(reg_mod_data_tidy)

spp_fct_levels <- reg_mod_data_tidy$p1 |> 
  left_join(xref_sci_names, by = c("species" = "common_name")) |> 
  mutate(compartment = factor(compartment, levels = c("benthic invert", "zooplankton", "fishes"))) |>
  arrange(compartment, TP_mode) |> 
  distinct(compartment, species) |> 
  mutate(species = factor(species, levels = species))


make_cred_plot <- function(data) {

  p_dat <- data |> 
    left_join(spp_fct_levels, by = "species") |> 
    mutate(species = factor(species, levels = spp_fct_levels$species))
  
  plot <- 
    (p_dat |>
       ggplot(aes(species, TP_mode, color = compartment, shape = baseline)) +
       # geom_errorbar(aes(ymin = TP_lower, ymax = TP_upper), width = 0, position = position_dodge(width=0.5), alpha = 0.7) + 
       geom_point(size = 2, position=position_dodge(width=0.5)) + 
       scale_x_discrete(labels = scales::wrap_format(10)) + 
       scale_color_manual(values = MetBrewer::met.brewer("Egypt", 3)) + 
       scale_shape_manual(values = c(0,1,2,4,5,6,7,13,15,16,17,18)) + 
       theme_clean() + 
       theme(axis.text.x = element_blank(), legend.position = "none") +
       labs(x = "", y = "posterior TP") ) / 
    (p_dat |> 
       ggplot(aes(species, Alpha_mode, color = compartment, shape = baseline)) +
       # geom_errorbar(aes(ymin = Alpha_lower, ymax = Alpha_upper), width = 0, position = position_dodge(width=0.5), alpha = 0.7) + 
       geom_point(size = 2, position=position_dodge(width=0.5)) + 
       scale_color_manual(values = MetBrewer::met.brewer("Egypt", 3)) +  
       scale_shape_manual(values = c(0,1,2,4,5,6,7,13,15,16,17,18)) + 
       theme_clean() + 
       theme(axis.text.x = element_text(angle = 75, hjust = 1))+
       labs(x = "Taxonomic group", y = "posterior alpha") ) + 
    plot_layout(guides = "collect") 
  plot
}

data_for_cred_plots <- reg_mod_data_tidy[c("p1","s1","s2","s3","s4")]
cred_plots <- data_for_cred_plots |> map(make_cred_plot)
cred_plots[[5]]

# Make plots for pooled ------------------------------------

p_dat <- reg_mod_data_tidy$p2 |> 
  left_join(spp_fct_levels, by = "species") |> 
  mutate(baseline = basin) |>
  # mutate(baseline = lake_region) |>
  # mutate(baseline = paste(basin, season, sep = "_")) |>
  # mutate(baseline = paste(lake_region, season, sep = "_")) |>
  mutate(species = factor(species, levels = spp_fct_levels$species))

p.p2 <- (p_dat |>
    ggplot(aes(species, TP_mode, color = compartment, shape = baseline)) +
    # geom_errorbar(aes(ymin = TP_lower, ymax = TP_upper), width = 0, position = position_dodge(width=0.5), alpha = 0.7) + 
    geom_point(size = 2, position=position_dodge(width=0.5)) + 
    scale_x_discrete(labels = scales::wrap_format(10)) + 
    scale_color_manual(values = MetBrewer::met.brewer("Egypt", 3)) + 
    scale_shape_manual(values = c(0,1,2,4,5,6,7,13,15,16,17,18)) + 
    theme_clean() + 
    theme(axis.text.x = element_blank(), legend.position = "none") +
    labs(x = "", y = "posterior TP") ) / 
  (p_dat |> 
     ggplot(aes(species, Alpha_mode, color = compartment, shape = baseline)) +
     # geom_errorbar(aes(ymin = Alpha_lower, ymax = Alpha_upper), width = 0, position = position_dodge(width=0.5), alpha = 0.7) + 
     geom_point(size = 2, position=position_dodge(width=0.5)) + 
     scale_color_manual(values = MetBrewer::met.brewer("Egypt", 3)) +   theme_clean() + 
     scale_shape_manual(values = c(0,1,2,4,5,6,7,13,15,16,17,18)) + 
     theme(axis.text.x = element_text(angle = 75, hjust = 1))+
     labs(x = "Taxonomic group", y = "posterior alpha") ) + 
  plot_layout(guides = "collect") 


# Save files ====================

panel_plots <- list(
  cred_plots[[1]], cred_plots[[2]], cred_plots[[3]], cred_plots[[4]], cred_plots[[5]], 
  p.p2, p.p4, p.p3, p.p5
  )
names(panel_plots) <- c(
  "tp_post_scale01_p1", 
  "tp_post_scale02a_s1", 
  "tp_post_scale02b_s2",
  "tp_post_scale03a_s3", 
  "tp_post_scale03b_s4",
  "tp_post_scale02a_p2", 
  "tp_post_scale02b_p4", 
  "tp_post_scale03a_p3", 
  "tp_post_scale03b_p5"
  )
paths <- stringr::str_c(names(panel_plots), ".png")
pwalk(list(paths, panel_plots), ggsave, 
      path = here("out", "plots"), height = 8, width = 10, 
      device = png)








# ====================

reg_mod_data_tidy$p5 |> 
  left_join(xref_sci_names, by = c("species" = "common_name")) |> 
  select(compartment, species, lake_region, season, TP_mode, Alpha_mode, mass_g) |> 
  arrange(compartment, species, lake_region, season) 

reg_mod_data_tidy$s4 |> 
  left_join(xref_sci_names, by = c("species" = "common_name")) |> 
  select(compartment, species, lake_region, season, TP_mode, Alpha_mode, mass_g) |> 
  arrange(compartment, species, lake_region, season) 

reg_mod_data_tidy$p5 |> 
  left_join(xref_sci_names, by = c("species" = "common_name")) |> 
  select(compartment, species, lake_region, season, TP_mode, Alpha_mode, mass_g) |> 
  filter(species %in% c("chinook salmon", "coho salmon", "lake trout", "burbot")) |> 
  arrange(species, Alpha_mode) |> 
  View()

reg_mod_data_tidy$p5 |> 
  group_by(species) |> 
  summarise(
    meanTP = mean(TP_mode), 
    minTP = min(TP_mode),
    maxTP = max(TP_mode),
    rangeTP = max(TP_mode)-min(TP_mode), 
    meanAlpha = mean(Alpha_mode), 
    minAlpha = min(Alpha_mode),
    maxAlpha = max(Alpha_mode),
    rangeAlpha = max(Alpha_mode)-min(Alpha_mode)
  ) |> 
  filter(species %in% c(
    "chinook salmon","coho salmon","lake trout","burbot","brown trout","rainbow trout")
    # "alewife")
    ) |> 
  arrange(meanAlpha)



