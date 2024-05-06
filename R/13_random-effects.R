
names(brm_mods_list)


mod <- brm_mods_list[["brm_mods_3b_s4_ind"]][[1]] 
print(mod)
posterior_summary(mod)


# Scale 2-specific
mod <- brm_mods_list[["brm_mods_2b_s2"]][[2]]
# forest(mod, grouping = c("lake_region"))
mod <- brm_mods_list[["brm_mods_2b_s2"]][[6]]
# forest(mod, grouping = c("lake_region"))
mod <- brm_mods_list[["brm_mods_2b_s2_ind"]][[1]]
# forest(mod, grouping = c("lake_region"))
mod <- brm_mods_list[["brm_mods_2b_s2"]][[11]]
# forest(mod, grouping = c("lake_region"))

# Scale 2-pooled
mod <- brm_mods_list[["brm_mods_2b_p4"]][[1]]
# forest(mod, grouping = c("lake_region"))
mod <- brm_mods_list[["brm_mods_2b_p4"]][[6]]
# forest(mod, grouping = c("lake_region"))
mod <- brm_mods_list[["brm_mods_2b_p4_ind"]][[1]]
# forest(mod, grouping = c("lake_region"))
mod <- brm_mods_list[["brm_mods_2b_p4"]][[11]]
# forest(mod, grouping = c("lake_region"))

# Scale 3-specific
mod <- brm_mods_list[["brm_mods_3b_s4"]][[1]]
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))
mod <- brm_mods_list[["brm_mods_3b_s4"]][[6]]
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))
mod <- brm_mods_list[["brm_mods_3b_s4_ind"]][[1]]
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))
mod <- brm_mods_list[["brm_mods_3b_s4"]][[11]] 
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))

# Scale 3-pooled
mod <- brm_mods_list[["brm_mods_3b_p5"]][[2]]
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))
mod <- brm_mods_list[["brm_mods_3b_p5"]][[8]]
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))
mod <- brm_mods_list[["brm_mods_3b_p5_ind"]][[1]]
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))
mod <- brm_mods_list[["brm_mods_3b_p5"]][[11]]
# forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))


# Scale 2-specific - asym --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_s2"]][[2]]

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_specific_asym.png"),
  height = 9, width = 18, units = "cm", device = png
)



# Scale 2-specific - TP ~ alpha^2 species --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_s2"]][[6]]
get_variables(mod)

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_specific_tp-a_species.png"),
  height = 9, width = 18, units = "cm", device = png
)


# Scale 2-specific - TP ~ alpha^2 ind --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_s2_ind"]][[1]]
get_variables(mod)

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_specific_tp-a_ind.png"),
  height = 9, width = 18, units = "cm", device = png
)

# Scale 2-specific - mass ~ alpha^2 spp --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_p4"]][[11]]
get_variables(mod)

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_specific_mass-a_species.png"),
  height = 9, width = 18, units = "cm", device = png
)


# Scale 2-pooled - asym --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_p4"]][[1]]
get_variables(mod)

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_pooled_asym.png"),
  height = 9, width = 18, units = "cm", device = png
)



# Scale 2-pooled - TP ~ alpha^2 species --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_p4"]][[6]]
get_variables(mod)

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_pooled_tp-a_species.png"),
  height = 9, width = 18, units = "cm", device = png
)


# Scale 2-pooled - TP ~ alpha^2 ind --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_p4_ind"]][[1]]
get_variables(mod)

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_pooled_tp-a_ind.png"),
  height = 9, width = 18, units = "cm", device = png
)

# Scale 2-pooled - mass ~ alpha^2 spp --------------------------------------------

mod <- brm_mods_list[["brm_mods_2b_p4"]][[11]]
get_variables(mod)

# region-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

ggsave(
  here("out","plots","R1","ranef_s2_pooled_mass-a_species.png"),
  height = 9, width = 18, units = "cm", device = png
)





# Scale 3-specific - asym --------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_s4"]][[1]]
get_variables(mod)

# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

p1 | p2

ggsave(
  here("out","plots","R1","ranef_s3_specific_asym.png"),
  height = 9, width = 30, units = "cm", device = png
)


# Scale 3-specific - TP ~ alpha^2 species --------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_s4"]][[6]]
get_variables(mod)

# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

p1 | p2

ggsave(
  here("out","plots","R1","ranef_s3_specific_tp-a_species.png"),
  height = 9, width = 30, units = "cm", device = png
)


# Scale 3-specific - TP ~ alpha^2 ind --------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_s4_ind"]][[1]]
get_variables(mod)

# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

p1 | p2

ggsave(
  here("out","plots","R1","ranef_s3_specific_tp-a_ind.png"),
  height = 9, width = 30, units = "cm", device = png
)

# Scale 3-specific - mass ~ alpha^2 spp --------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_s4"]][[11]] 
get_variables(mod)

# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

p1 | p2

ggsave(
  here("out","plots","R1","ranef_s3_specific_mass-a_species.png"),
  height = 9, width = 30, units = "cm", device = png
)


# Scale 3-pooled - asym --------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_p5"]][[2]]
get_variables(mod)

# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

p1 | p2

ggsave(
  here("out","plots","R1","ranef_s3_pooled_asym.png"),
  height = 9, width = 30, units = "cm", device = png
)


# Scale 3-pooled - TP ~ alpha^2 species--------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_p5"]][[8]]
get_variables(mod)

# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  filter(term == "Intercept") |> 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal() 
p1

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  filter(term == "Intercept") |> 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()
p2

# IAlpha_modeE2 - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_IAlpha_modeE2) %>% 
  filter(term == "IAlpha_modeE2") |> 
  mutate(b_IAlpha_modeE2 = r_lake_region + b_IAlpha_modeE2) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_IAlpha_modeE2) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_IAlpha_modeE2)

p3 <- out_all %>%   
  ggplot(aes(b_IAlpha_modeE2, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_IAlpha_modeE2} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal() 
p3

# IAlpha_modeE2 - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_IAlpha_modeE2) %>% 
  filter(term == "IAlpha_modeE2") |> 
  mutate(b_IAlpha_modeE2 = r_season + b_IAlpha_modeE2) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_IAlpha_modeE2) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_IAlpha_modeE2)

p4 <- out_all %>%   
  ggplot(aes(b_IAlpha_modeE2, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_IAlpha_modeE2} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()
p4

(p1 | p3) / (p2 | p4)

ggsave(
  here("out","plots","R1","ranef_s3_pooled_tp-a_species.png"),
  height = 14, width = 30, units = "cm", device = png
)

# Scale 3-pooled - TP ~ alpha^2 ind ------------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_p5_ind"]][[1]]
get_variables(mod)


# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

p1 | p2

ggsave(
  here("out","plots","R1","ranef_s3_pooled_tp-a_ind.png"),
  height = 9, width = 30, units = "cm", device = png
)

# Scale 3-pooled - mass ~ alpha^2 spp --------------------------------------------

mod <- brm_mods_list[["brm_mods_3b_p5"]][[11]]
get_variables(mod)

# intercept - region
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

# intercept - season
out_r_reg <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
  mutate(b_Intercept = r_season + b_Intercept) |> 
  rename(group = season, ranef = r_season)

out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

out_all <- bind_rows(out_r_reg, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

p2 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()

p1 | p2

ggsave(
  here("out","plots","R1","ranef_s3_pooled_mass-a_species.png"),
  height = 9, width = 30, units = "cm", device = png
)




