#-----------------------------------------------#
#
# Prepare and explore SI data from CSMI 2015
# Author: Dr. Bryan M Maitland
# Email: bmaitland101@gamil.com
#
#----------------------------------------------#

## Libraries
library(tidyverse)
library(here)
source(here("R/xrefs.R"))


## Data ----

# CSMI 2015 raw data
dat_csmi_2015 <- 
  readxl::read_xlsx(
    here("data-raw/CSMI-2015-data.xlsx"), 
    sheet = "Combined UF MED"
    )

# Clean up columns using definitions tab in .xlsx file
dat_csmi_2015 <- 
  dat_csmi_2015 |> 
  # remove columns we don't need
  select(
    -Depth_O,  # ???
    -Tray,  # loading tray 
    -Lab,  # ID of SI lab
    -Line,  # column from SI lab
    -`OP Notes`,  # comments from SI lab
    -Comments,  # general comments
    -Species_O  # ???
    )|> 
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

# check it  
dat_csmi_2015

# save cleaned data
# write_rds(dat_csmi_2015, here("data/dat_csmi_2015.rds"))


## Explore -----

# Data overview
dat_csmi_2015 |> skimr::skim()

# Notes:
# 1,792 rows, but only 1,619 unique sampleIDs... indicates duplicates
# 169 samples (10%) with unknown sample type... could fix with species? 
# 8 samples (1%) form unknown sites, probably the same 8 that don't have spp IDs
# 241 samples do not have a matched up site name, so check cross reference.
# 12 samples missing season information
# Only 1% do not have isotope ratio data, but 15% don't have corrected C values






