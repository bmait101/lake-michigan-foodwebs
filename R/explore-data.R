
p.dat <- dat_csmi2015 |> 
  mutate(spp_code = case_when(
    spp_code=="AIP"~"ALE", spp_code=="BIP"~"BLO", TRUE ~as.character(spp_code)))

## Shotgun plots ------

# All data
p.dat |> 
  ggplot(aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = sample_type))

# Fish and invert - Hoffman lipid correction
p.dat |> 
  filter(d13Cc < 0) |> 
  ggplot(aes(x = d13Cc, y = d15N)) + 
  geom_point(aes(color = sample_type))
  
# Fish
p.dat |> 
  filter(sample_type == "fish") |> 
  ggplot(aes(x = d13Cc, y = d15N)) + 
  geom_point(aes(color = spp_code))

p.dat |> 
  filter(spp_code %in% c("ALE", "BLO", "ROG", "DWS", "RAS", "SMS")) |> 
  ggplot(aes(x = d13Cc, y = d15N,  color = spp_code)) + 
  # geom_point() +
  stat_ellipse(geom = "polygon",
               aes(fill = spp_code), 
               alpha = 0.2)
