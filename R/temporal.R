

data |> 
  filter(dataset=="oreilly_2014_15") |> 
  group_by(year, species) |> 
  summarise(meanC = mean(d13c_norm, na.rm = TRUE), meanN = mean(d15n, na.rm = TRUE), 
            sdC = sd(d13c_norm, na.rm = TRUE), sdN = sd(d15n, na.rm = TRUE)) |> 
  ggplot(aes(meanC, meanN)) + 
  geom_point(size = 3) + 
  facet_grid(vars(year)) + 
  geom_errorbar(aes(ymin = meanN - sdN, ymax = meanN + sdN)) +
  geom_errorbarh(aes(xmin = meanC - sdC, xmax = meanC + sdC)) +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
  theme_bw()

data |> 
  filter(dataset=="oreilly_2014_15") |> 
  ggplot(aes(d13c_norm, d15n, color = species)) + 
  geom_point() + 
  facet_grid(vars(year))


df <- data |> 
  filter(year %in% c(2014,2015,2016)) |> 
  filter(!compartment %in% c("ichthoplankton"))

# remove extreme outliers
df |> filter(d13c_norm < -40)
df |> filter(d13c_norm > -12)
df <- filter(df, ! d13c_norm < -40)
df <- filter(df, ! d13c_norm > -12)

df_means <- df |> 
  group_by(year, species) |> 
  summarise(meanC = mean(d13c_norm, na.rm = TRUE), meanN = mean(d15n, na.rm = TRUE), 
            sdC = sd(d13c_norm, na.rm = TRUE), sdN = sd(d15n, na.rm = TRUE)) 


df_means |> 
  # filter(species=="chinook salmon") |>
  ggplot(aes(meanC, meanN, color = factor(year))) + 
  # geom_point(size = 3) + 
  geom_point(
    data=df,
    aes(d13c_norm, d15n, color = factor(year)), alpha = 0.2, size = 1.5
    ) + 
  geom_linerange(aes(ymin = meanN - sdN, ymax = meanN + sdN), linewidth = 1) +
  geom_linerange(aes(xmin = meanC - sdC, xmax = meanC + sdC), linewidth = 1) +
  facet_wrap(vars(species)) +
  # ggrepel::geom_text_repel(aes(label = species), max.overlaps = 50, size=3) +
  theme_bw()
