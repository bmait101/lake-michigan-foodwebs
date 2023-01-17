
# Data for analysis
df <- data %>%
  filter(dataset %in% c("csmi_2015","uwm_2010_2011")) |>
  filter(!compartment %in% c("pom", "macro-alga")) |>
  droplevels() 
  
## Visualize relationship between isotope ratio values and depth

# function for plots
scatter <- function(df, depth_m, isotope, color, facet){
  df |> 
    ggplot(aes({{ depth_m }}, {{ isotope }}, color = {{color}})) +
    geom_point() + 
    geom_smooth(se = FALSE, method = "lm") +
    # facet_grid(cols = vars(factor({{cols}}))) + 
    facet_wrap(vars({{facet}})) + 
    scale_x_continuous(breaks = c(0,30,100,200,300))
  }

df |> scatter(depth_m, d15n, compartment, species) 
df |> scatter(depth_m, d13c_norm, compartment, species) 


df |> 
  # filter(compartment %in% c("dreissenids","benthic inverts")) |>
  geom_point() +
  geom_smooth(method = "lm") 







# means <- df |> 
#   filter(species %in% c("lake trout", "slimy sculpin")) |>
#   group_by(species) |> 
#   summarise(meanC = mean(d13c_norm), sdC = sd(d13c_norm), 
#             meanN = mean(d15n), sdN = sd(d15n))
# df |> 
#   filter(species %in% c("lake trout", "slimy sculpin")) |>
#   ggplot(aes(d13c_norm, d15n)) + 
#   geom_point(aes(color= species)) + 
#   geom_point(data = means, aes(meanC, meanN)) 



# RUN MODELS ===================================================================

library(broom)

df_mod <- df  

# Fit lm models to data
fit <- lm(d15n ~ depth_m, data = df_mod)
summary(fit)

resid <- modelr::add_residuals(df_mod, fit) |> as_tibble()

resid |> 
  ggplot(aes(depth_m, d15n)) + 
  geom_point() + 
  geom_smooth(method = "lm") |
  
  resid |> 
    ggplot(aes(depth_m, resid)) + 
    geom_point() + 
    geom_smooth(method = "lm")


## By group
df_nested <- df_mod |> 
  group_by(species) |> 
  filter(n()>5) |> 
  nest() |> 
  mutate(
    fitN = map(data, ~lm(d15n ~ depth_m, data = .x)),
    fitC = map(data, ~lm(d13c_norm ~ depth_m, data = .x))
  ) |> 
  gather(mod_name, model, fitN:fitC) |>
  mutate(
    tidied = map(model, glance), 
    resids = map2(data, model, modelr::add_residuals))


df_nested |> unnest(tidied)

df_nested |> unnest(tidied) |> 
  ggplot(aes(reorder(species, p.value), p.value, color = mod_name)) + 
  geom_point() + 
  geom_hline(yintercept = 0.05) + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))

df_nested |>
  unnest(resids) |> filter(mod_name == "fitN") |> 
  group_by(species) |> 
  summarise(mean_res = mean(resid))

df_nested |>
  unnest(resids) |> filter(mod_name == "fitN") |>
  ggplot(aes(depth_m, d15n)) + 
  geom_point() + 
  geom_smooth(method = "lm") |

df_nested |>
  unnest(resids) |> filter(mod_name == "fitN") |>
  ggplot(aes(depth_m, resid)) +
  # facet_grid(cols = vars(species)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

# ==========


## Prep data ============

df <- df_nested |>
  unnest(resids) |> 
  filter(mod_name == "fitN") |> 
  # select(
  #   dataset, lake_region, year, season, depth_m, 
  #   compartment, species, species_group, d13c_norm, d15n, length_mm, mass_g
  # ) |> 
  drop_na(d13c_norm, d15n) |> 
  filter(dataset %in% c("csmi_2015", "nps_2015")) |>
  filter(!compartment %in% c("ichthoplankton", "pom", "macro-alga")) |>
  mutate(lake_region = "pooled") |>
  # mutate(lake_region = lake_region) |>
  # mutate(trophic = case_when(
  #   compartment == "pom" ~ "b1", 
  #   compartment == "macro-alga" ~ "b2",
  #   TRUE ~ "consumer"
  # )) |>
  mutate(trophic = case_when(
    compartment == "dreissenids" ~ "b1",
    species_group %in% c("amphipod")  ~ "b2",
    TRUE ~ "consumer"
  )) |>
  droplevels() |> 
  as.data.frame()

df |> 
  ggplot(aes(d13c_norm, resid)) + 
  geom_point(aes(color = trophic), size = 2, alpha = .5) + 
  facet_wrap(vars(lake_region)) +
  labs(color = "Trophic Group", shape = "Trophic Group")


## TP and alpha ===============

IsotopesList <- extractIsotopeData(
  df,
  b1 = "b1",
  b2 = "b2", 
  baselineColumn = "trophic", 
  consumersColumn = "species",
  groupsColumn = "lake_region",
  d13C = "d13c_norm", 
  d15N = "d15n"
)

summary(IsotopesList)

# Run two baseline model (Increase n.iter, burn in, thin 4 model convergence)
TP_model <- parLapply(
  cl, IsotopesList, multiModelTP,
  model = "twoBaselinesFull", lambda = 1,
  print = TRUE, n.chains = 5, n.iter = 1000, burnin = 100, thin = 1
) 

# Summarize TP data
TP_data <- fromParallelTP(TP_model, get = "summary")
head(TP_data)
colnames(TP_data) <- c(
  'model','lake_region',"species",
  'TP_lower','TP_upper','TP_median','TP_mode',
  'Alpha_lower','Alpha_upper','Alpha_median','Alpha_mode'
)

# Data for models: average body size, merge TP data, and log transform body mass
df_mod <- df %>%
  filter(trophic == "consumer") %>%
  select(species, lake_region, mass_g) %>%
  group_by(species, lake_region) %>%
  summarise(mass_g = mean(mass_g, na.rm = TRUE), .groups = 'drop') %>%
  left_join(TP_data, by=c("species", "lake_region")) %>%
  mutate(
    lake_region = factor(lake_region),
    log_mass = log(mass_g)
  ) %>%
  as.data.frame ()


# Plot data
df_mod |> 
  ggplot(aes(Alpha_mode, TP_mode)) +
  # ggplot(aes(Alpha_mode, mass_g)) +
  # ggplot(aes(log_length, TP_mode)) +
  # geom_point(size = 3, alpha = 0.5, shape = 21) +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", color = "black") +
  geom_point(aes(fill = lake_region), size = 3, alpha = 0.5, shape = 21) +
  scale_y_log10(labels = scales::label_comma()) +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) 







## By group
df_nested <- df_mod |> 
  filter(compartment %in% c("dreissenids","benthic inverts")) |>
  filter(!species %in% c("crayfish","leech","hydracarina")) |>
  group_by(species) |> 
  # filter(n()>5) |> 
  nest() |> 
  mutate(
    fit = map(data, ~glm(d15n ~ d13c_norm, data = .x, family = "binomial")),
  ) |> 
  # gather(mod_name, model, fitN:fitC) |>
  mutate(
    tidied = map(fit, glance), 
    resids = map2(data, fit, modelr::add_residuals))



df_nested |>
  unnest(resids) |> 
  ggplot(aes(d13c_norm, d15n, color = species)) + 
  geom_point() 
  
df_nested |>
  unnest(resids) |> 
  ggplot(aes(d13c_norm, resid, color = species)) + 
  # facet_grid(cols = vars(species)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")  + 
  plot_layout(guides = 'collect')



df_mod <- df |> 
  filter(compartment %in% c("dreissenids","benthic inverts")) |>
  filter(!species %in% c("crayfish","leech","hydracarina"))
df_mod |>
  ggplot(aes(d13c_norm, d15n, color = species)) + 
  geom_point() 

xs <- seq(-35, -10, length.out = 80)
# Create the curve from the equation parameters
trend <- tibble(
  d13c_norm = xs,
  asymptote = 12.5,
  scale = .3,
  midpoint = -20,
  d15n = asymptote / (1 + exp(9 + (.4 * d13c_norm))))

df_mod |>
  ggplot(aes(d13c_norm, d15n)) + 
  geom_point()  + 
  geom_line(data = trend) 


data |>
  select(compartment, species, d13c_norm, d15n) |> 
  filter(compartment %in% c("fishes")) |>
  mutate(d15n_c = 12.5 / (1 + exp(9 + (.4 * d13c_norm)))) |>
  mutate(TP = ((d15n - d15n_c)/3.4) + 2) |> 
  group_by(species) |> 
  summarise(meanTP = mean(TP), meanC = mean(d13c_norm)) |> 
  ggplot(aes(meanC, meanTP)) + 
  geom_point() + 
  ggrepel::geom_label_repel(aes(label = species))




