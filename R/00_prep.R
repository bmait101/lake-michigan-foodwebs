# Load / install libraries, helpers, xrefs

pacman::p_load(
  here,  # file paths
  tidyverse,  # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  readxl, # read excels files
  patchwork, # plot composition
  ggnewscale, # use multiple scales in ggplot2
  MetBrewer, # artsy palettes
  fishualize,  # fishy palletts
  janitor,  # cleaning data
  visdat,  # data QC
  skimr,  # data QC
  parallel,  # support for parallel computing
  tRophicPosition,  # Bayesian Trophic Position Estimation with Stable Isotopes 
  brms,  # Bayesian Regression Models using 'Stan'
  brmstools, #  post-processing functions for brmsfit object
  rstan,  # R interface to Stan
  rstanarm,  # rstanarm: Bayesian Applied Regression Modeling via Stan
  tidybayes, # tidy data + ggplot workflow for bayes
  broom,  # tidy statistical objects
  performance # r2_bayes function
  # conflicted
)

# conflict_prefer("filter", "dplyr")
# conflict_prefer("everything", "dplyr")
# conflict_prefer("ar", "stats")
# conflict_prefer("chisq.test", "stats")
# conflict_prefer("dirichlet", "brms")
# conflict_prefer("exponential", "brms")
# conflict_prefer("extract", "rstan")
# conflict_prefer("fisher.test", "stats")

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





