df_base |> 
  filter(compartment %in% c("pom", "macro-alga")) |> 
  group_by(compartment, season) |> 
  summarise(c = mean(d13c), 
            c_sd = sd(d13c), 
            n = mean(d13c), 
            n_sd = sd(d13c))

tmp <- df_base |> filter(compartment %in% c("macro-alga")) 
anova(lm(d13c ~ season, data = tmp))
anova(lm(d15n ~ season, data = tmp))

anova(lm(d13c ~ lake_region, data = tmp))
anova(lm(d15n ~ lake_region, data = tmp))

anova(lm(d13c ~ depth_g, data = tmp))
anova(lm(d15n ~ depth_g, data = tmp))


tmp <-   df_base |> filter(compartment %in% c("pom"))
anova(lm(d13c ~ season, data = tmp))
anova(lm(d15n ~ season, data = tmp))

anova(lm(d13c ~ lake_region, data = tmp))
anova(lm(d15n ~ lake_region, data = tmp))

anova(lm(d13c ~ depth_g, data = tmp))
anova(lm(d15n ~ depth_g, data = tmp))

