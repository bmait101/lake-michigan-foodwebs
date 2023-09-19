
names(brm_mods_list_asym)
names(brm_mods_list)


mod <- brm_mods_list_asym[["brm_mods_3b_s4_asym"]][[2]]
print(mod)
posterior_summary(mod)


# Scale 2-specific
mod <- brm_mods_list_asym[["brm_mods_2b_s2_asym"]][[2]]
forest(mod, grouping = c("lake_region"))

mod <- brm_mods_list[["brm_mods_2b_s2"]][[6]]
forest(mod, grouping = c("lake_region"))

mod <- brm_mods_list[["brm_mods_2b_s2_ind"]][[1]]
forest(mod, grouping = c("lake_region"))

mod <- brm_mods_list[["brm_mods_2b_s2"]][[11]]
forest(mod, grouping = c("lake_region"))


# Scale 2-pooled
mod <- brm_mods_list_asym[["brm_mods_2b_p4_asym"]][[1]]
forest(mod, grouping = c("lake_region"))

mod <- brm_mods_list[["brm_mods_2b_p4"]][[6]]
forest(mod, grouping = c("lake_region"))

mod <- brm_mods_list[["brm_mods_2b_p4_ind"]][[1]]
forest(mod, grouping = c("lake_region"))

mod <- brm_mods_list[["brm_mods_2b_p4"]][[11]]
forest(mod, grouping = c("lake_region"))


# Scale 3-specific
mod <- brm_mods_list_asym[["brm_mods_3b_s4_asym"]][[1]]
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))

mod <- brm_mods_list[["brm_mods_3b_s4"]][[6]]
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))

mod <- brm_mods_list[["brm_mods_3b_s4_ind"]][[1]]
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))

mod <- brm_mods_list[["brm_mods_3b_s4"]][[11]] 
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))


# Scale 3-pooled
mod <- brm_mods_list_asym[["brm_mods_3b_p5_asym"]][[4]]
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))

mod <- brm_mods_list[["brm_mods_3b_p5"]][[8]]
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))

mod <- brm_mods_list[["brm_mods_3b_p5_ind"]][[1]]
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))

mod <- brm_mods_list[["brm_mods_3b_p5"]][[11]]
forest(mod, grouping = c("lake_region"))
forest(mod, grouping = c("season"))















# Study-specific effects are deviations + average
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
    # Use glue package to combine strings
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()




# Scale 2-specific

mod <- brm_mods_list_asym[["brm_mods_2b_s2_asym"]][[2]]

forest(mod, grouping = c("lake_region"))
# forest(mod, grouping = c("season"))

# Study-specific effects are deviations + average
out_r_reg <- spread_draws(mod, r_lake_region[lake_region,term], b_Intercept) %>% 
  mutate(b_Intercept = r_lake_region + b_Intercept) |> 
  rename(group = lake_region, ranef = r_lake_region)

# out_r_ses <- spread_draws(mod, r_season[season,term], b_Intercept) %>% 
#   mutate(b_Intercept = r_season + b_Intercept) |> 
#   rename(group = season, ranef = r_season) |> 
#   mutate(group = as.character(group))

# Average effect
out_f <- spread_draws(mod, b_Intercept) %>% 
  mutate(group = "Average")

# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r_reg, out_r_ses, out_f) %>% 
  ungroup() %>%
  mutate(group = fct_relevel(group, "Average"))

# Data frame of summary numbers
out_all_sum <- out_all |> 
  group_by(group) %>% 
  mean_qi(b_Intercept)

# Draw plot
p1 <- out_all %>%   
  ggplot(aes(b_Intercept, group)) +
  ggridges::geom_density_ridges(rel_min_height = 0.01, col = NA,scale = 1) +
  geom_pointinterval(data = out_all_sum, aes(xmin = .lower, xmax = .upper), size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    # Use glue package to combine strings
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  ) +
  theme_minimal()




