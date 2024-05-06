# Load / install libraries, helpers, xrefs

library(here)  # file paths
library(tidyverse)  # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl) # read excels files
library(patchwork) # plot composition
library(ggnewscale) # use multiple scales in ggplot2
library(MetBrewer) # artsy palettes
library(fishualize) # fishy palletts
library(janitor) # cleaning data
library(visdat) # data QC
library(skimr) # data QC
library(parallel)  # support for parallel computing
# library(tRophicPosition)  # Bayesian Trophic Position Estimation with Stable Isotopes 
library(brms) # Bayesian Regression Models using 'Stan'
# library(brmstools) #  post-processing functions for brmsfit object
library(rstan)  # R interface to Stan
library(rstanarm) # rstanarm: Bayesian Applied Regression Modeling via Stan
library(tidybayes) # tidy data + ggplot workflow for bayes
library(broom)  # tidy statistical objects
library(broom.mixed)  # tidy mixed models
library(performance) # r2_bayes function


# Source helper fxs
source(here("R", "helper_fxs.R"))  # helper functions for data cleaning
# source(here("R", "r2_bayes.R"))  

# Source xrefs tables for ports, species
xref_ports <- 
  read_csv(here("out", "xrefs", "xref-sites-ports-regions.csv")) |> 
  cleans_names_and_caps()

xref_compartments <- 
  read_csv(here("out", "xrefs", "xref_taxa_compartment.csv")) |> 
  cleans_names_and_caps()

# load scientific names
xref_sci_names <- 
  read.csv(here("out", "xrefs", "xref_species_names.csv")) |> 
  mutate(common_name = str_to_lower(common_name))





