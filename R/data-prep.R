# Prep RAW data

# libraries
library(tidyverse)
library(here)
source(here("R/xrefs.R"))


# CSMI 2015 stable isotope ratio data ----

# read in flat data
dat_csmi_2015 <- 
  readxl::read_xlsx(
    here("data-raw/CSMI-2015-data.xlsx"), 
    sheet = "Combined UF MED"
    )

# clean columns via definitions sheet
dat_csmi_2015 <- 
  dat_csmi_2015 |> 
  select(-Depth_O, -Tray, -Lab, -Line, -`OP Notes`, -Comments, -Species_O) |> 
  rename(
    sampleID = `Sample ID`, 
    sample_type = `F/I`,  # f=fish, i=invert, z=standard
    site_code = Site,  # 3-letter codes
    season = Season,  # 1=may, 2=june/july, 3=aug/sep
    depth_m = `Depth(m)`,
    spp_code = Species, 
    d13C = d13Cb,
    cn = `C:N`,
    wtN = `wt%N`, 
    wtC = `wt%C`
    ) |> 
  mutate(
    sample_type = case_when(
      sample_type == "F" ~ "fish", 
      sample_type == "I" ~ "invert", 
      TRUE ~ NA_character_
      ), 
    season = case_when(
      season == 1 ~ "May", 
      season == 2 ~ "Jun/Jul", 
      season == 3 ~ "Aug/Sep", 
      TRUE ~ NA_character_
    )) |> 
  left_join(site_ref, by = "site_code") |> 
  left_join(spp_ref, by = "spp_code") |> 
  relocate(d13C, .after = d15N) |> 
  relocate(site_name, .after = site_code) |> 
  relocate(spp_name, .after = spp_code) |> 
  mutate(across(2:7, as_factor))
  
dat_csmi_2015

summary(dat_csmi_2015)


dat_csmi_2015 |> 
  group_by(spp_name) |> 
  tally() |> arrange(spp_name) |> print(n=Inf) 

