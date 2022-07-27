#-----------------------------------------------#
#
# Prepare and clean SI data from CSMI 2015
# Author: Dr. Bryan M Maitland
# Email: bmaitland101@gamil.com
#
#----------------------------------------------#

## Libraries
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2); theme_set(theme_bw())
library(patchwork)
library(purrr)
library(here)
source(here("R/xrefs.R"))


## Data ----

# CSMI 2015 raw data
dat_csmi2015_raw <- 
  readxl::read_xlsx(
    here("data","CSMI-2015-rawdata.xlsx"), 
    sheet = "Combined UF MED"
    )

# Site xref
site_xref <- readr::read_csv(here("data", "site-xref.csv"))


# Data overview
dat_csmi2015_raw |> skimr::skim()


## Clean -----

### Tidy columns ----
dat_csmi2015 <- dat_csmi2015_raw |> 
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
  left_join(site_xref |> select(site_code, site_name), by = "site_code") |> 
  left_join(spp_ref, by = "spp_code") |> 
  relocate(d13C, .after = d15N) |> 
  relocate(site_name, .after = site_code) |> 
  relocate(spp_name, .after = spp_code) |> 
  mutate(across(2:7, as_factor)) |> 
  mutate(season = fct_relevel(season, c('May', 'Jun/Jul', 'Aug/Sep')))


# Check it
dat_csmi2015 |> skimr::skim()

# Issues to fix:
# 1. missing data
# 2. duplicate samples

### Missing data -----

# Inspect samples with missing data
# dat_csmi2015 |> filter(is.na(sample_type)) |> distinct(spp_name)
# dat_csmi2015 |> filter(is.na(site_code))
# dat_csmi2015 |> filter(is.na(site_name))
# dat_csmi2015 |> filter(is.na(season))
# dat_csmi2015 |> filter(is.na(depth_m))
# dat_csmi2015 |> filter(is.na(d15N))
# dat_csmi2015 |> filter(is.na(d13C))

# Notes:
# Samples with unknown type are all POM
# Samples with missing site data also missing most other info
#   also unclear what site "0" is... only three samples so removing them
# 12 samples missing season information

# Remove samples with missing data and assign POM sample types
dat_csmi2015 <- dat_csmi2015 |> 
  drop_na(d15N, d13C, depth_m, site_code, site_name, season) |> 
  filter(!site_code == "Z.unk") |> 
  filter(!site_name == "Unknown") |> 
  droplevels() |> 
  mutate(sample_type = coalesce(sample_type, "POM")) 

# Check it
dat_csmi2015 |> skimr::skim()


### Duplicate samples -----

# Table of all duplicate rows:
dat_csmi2015_dupes <- dat_csmi2015 |> janitor::get_dupes(sampleID)

# Keep only one row for each duplicated sample
dupes_to_add <- dat_csmi2015_dupes |> 
  group_by(sampleID) |> 
  slice_head(n = 1)

# Remove duplicate rows / add back single observation
dat_csmi2015 <- dat_csmi2015 |> 
  filter(!sampleID %in% unique(dat_csmi2015_dupes$sampleID)) |> 
  bind_rows(dupes_to_add) |> 
  select(-dupe_count)

# Check it
dat_csmi2015 |> skimr::skim()


## Visuals -----

# Map of sites
library(sf)
site_xref |> 
  filter(!site_code=="Z.unk") |> 
  st_as_sf(coords = c("lat", "long")) |> 
  ggplot() + 
  geom_sf()

### Samples by site and type ----
dat_csmi2015 |> 
  count(site_name, sample_type) |> 
  complete(site_name, sample_type, fill = list(n=0)) |> 
  ggplot(aes(x=site_name, y=n)) + 
  geom_bar(aes(fill = sample_type), position = "dodge", stat="identity") + 
  scale_fill_brewer(palette = "Set1") + 
  labs(title = "Count of samples by site and sample type", 
       x = "", y = "Number of samples", fill = "Sample Type")

# ~100 fish samples per site
# ~50-75 inverts per site
# ~10-15 POM per site


### Samples by site and season ----

#### Facet by species (a bit tight)
dat_csmi2015 |> 
  count(site_name, season) |> 
  complete(site_name, season, fill = list(n=0)) |> 
  ggplot(aes(x=site_name, y=n)) + 
  geom_bar(aes(fill = season), position = "dodge", stat="identity") + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(title = "Count of samples by site and season", 
       x = "", y = "Number of samples", fill = "Season")

#### Plot by species
plot.sample.counts <- function(df, target_species) {
  df |> 
    filter(spp_name == target_species) |> 
    count(site_name, season) |> 
    complete(site_name, season, fill = list(n=0)) |> 
    ggplot(aes(x=site_name, y=n)) + 
    geom_bar(aes(fill = season), position = "dodge", stat="identity") + 
    scale_fill_brewer(palette = "Dark2") + 
    labs(
      title = paste(target_species), 
      x = "", y = "No. samples", fill = "Season"
      )
}
# plot.sample.counts(df = dat_csmi2015, target_species = "Alewife")
spp_to_plot <- c("Alewife", "Algae", "Amphipod")
p.spp.sample.counts <- spp_to_plot |> 
  map(~ plot.sample.counts(df = dat_csmi2015, target_species = .x))

p.spp.sample.counts[[1]] /
  p.spp.sample.counts[[2]] /
  p.spp.sample.counts[[3]] + 
  plot_layout(guides = "collect")




