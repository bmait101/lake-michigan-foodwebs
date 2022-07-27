


## Shotgun plots ------

# All data
dat_csmi2015 |> 
  ggplot(aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = sample_type))
  
# Fish
dat_csmi2015 |> 
  filter(sample_type == "fish") |> 
  mutate(spp_code = case_when(
    spp_code=="AIP"~"ALE", spp_code=="BIP"~"BLO", TRUE ~as.character(spp_code))) |> 
  ggplot(aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = spp_code))

dat_csmi2015 |> 
  mutate(spp_code = case_when(
    spp_code=="AIP"~"ALE", spp_code=="BIP"~"BLO", TRUE ~as.character(spp_code))) |> 
  filter(spp_code %in% c("ALE", "YEP", "BLO")) |> 
  ggplot(aes(x = d13C, y = d15N)) + 
  geom_point(aes(color = spp_code))
