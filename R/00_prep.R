# Load / install libraries, helpers, xrefs

pacman::p_load(
  here,  # file paths
  tidyverse,  # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  readxl, # read excels files
  patchwork, # plot composition
  MetBrewer, # artsy palettes
  fishualize,  # fishy palletts
  janitor,  # cleaning data
  visdat,  # data QC
  skimr,  # data QC
  parallel,  # support for parallel computing
  tRophicPosition,  # Bayesian Trophic Position Estimation with Stable Isotopes 
  brms,  # Bayesian Regression Models using 'Stan'
  # devtools::install_github("mvuorre/brmstools")
  brmstools, #  post-processing functions for brmsfit object
  rstan,  # R interface to Stan
  rstanarm,  # rstanarm: Bayesian Applied Regression Modeling via Stan
  tidybayes, # tidy data + ggplot workflow for bayes
  broom,  # tidy statistical objects
  performance
)


# Source helper fxs
source(here("R", "helper_fxs.R"))  # helper functions for data cleaning
# source(here("R", "r2_bayes.R"))  

# Source xrefs tables for ports, species
source(here("R", "load_xref_tbls.R"))


theme_clean <- function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = NA), 
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = rel(1), hjust = 0),
      strip.background = element_rect(fill = "grey80", color = NA),
      legend.title = element_text(face = "bold"))
}



