#-----------------------------------------------#
# Prepare and clean SI data from Feiner et al. (2019) PLOS One
# Author: Dr. Bryan M Maitland
# Email: bmaitland101@gmail.com
#----------------------------------------------#

## Libraries
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2); theme_set(theme_bw())
library(patchwork)
library(purrr)
library(here)



## Data ----

# CSMI 2015 raw data
dat_feiner19_raw <- 
  readxl::read_xlsx(
    here("data","Feiner-et-al-2018-rawdata.xlsx"), 
    sheet = "Diet-StableIsotope"
  )|> 
  # remove columns we don't need
  select(
    FishID, Species, Site, Month, Depth, Substrate, Season, Loc, LocNum, TL,
    Corrected.d29_28, Corrected.d13_12, Corr.C, Corr.N
  ) |> 
  mutate(across(c(TL), as.numeric))


# Data overview
dat_feiner19_raw |> skimr::skim()


# d15N ~ Fish size
dat_feiner19_raw |> 
  filter(Species == "YEP") |> 
  ggplot(aes(log(TL), Corr.N)) +
  facet_wrap(vars(Species)) +
  geom_point(aes(fill = Corr.C), shape = 21) + 
  geom_smooth(method = "lm")



# Convert length to body mass

# LWR-parameters for species
yep <- read.csv()





