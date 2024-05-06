# Script to extract parameter estimates from Bayesian regression models

# Prep =========================================================================

# libraries
# source(here::here("R", "00_prep.R"))
library(here)        # file paths
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(brms)

# Data
load(file = here("out", "data", "reg_mod_data_v3.RData"))

# Model objects
load(file = here("out", "models", "brms", "brm_mods_list_20240327.RData"))
# load(file = here("out", "models", "brms", "brm_mods_list_v2.RData"))
# load(file = here("out", "models", "brms", "brm_mods_list_asym.RData"))

# Check 
names(brm_mods_list)
names(reg_mod_data_tidy)

# Extract parameter estimates ==================================================

# extract parameter
summary(brm_mods_list[["brm_mods_1"]][[1]])$fixed |> 
  rownames_to_column(var = 'Fixed Parameter') |> 
  mutate(across(where(is.numeric), ~round(., digits = 3))) |> 
  mutate(across(7:8, ~round(., digits = 0))) |>
  rename_all(~gsub("\\.", " ", .)) |> 
  rename_all(~gsub("\\_", " ", .)) 

# function to extract
make_param_tab <- function(model_object) {
  summary_tab <- summary(model_object)$fixed |> 
    rownames_to_column(var = 'Fixed Parameter') |> 
    mutate(across(where(is.numeric), ~round(., digits = 3))) |> 
    mutate(across(7:8, ~round(., digits = 0))) |> 
    rename_all(~gsub("\\.", " ", .)) |> 
    rename_all(~gsub("\\_", " ", .)) 
  summary_tab
}

# test
make_param_tab(brm_mods_list[["brm_mods_1"]][[1]])
make_param_tab(brm_mods_list[["brm_mods_2b_p4"]][[1]])

# list best fit model objects to loop
mods_for_tabs <- list(
  brm_mods_list[["brm_mods_1"]][[2]],
  brm_mods_list[["brm_mods_1"]][[4]],
  brm_mods_list[["brm_mods_1_ind"]][[1]],
  brm_mods_list[["brm_mods_1"]][[7]],
  
  brm_mods_list[["brm_mods_2b_s2"]][[2]],
  brm_mods_list[["brm_mods_2b_s2"]][[6]],
  brm_mods_list[["brm_mods_2b_s2_ind"]][[1]],
  brm_mods_list[["brm_mods_2b_s2"]][[11]],
  
  brm_mods_list[["brm_mods_2b_p4"]][[1]],
  brm_mods_list[["brm_mods_2b_p4"]][[6]],
  brm_mods_list[["brm_mods_2b_p4_ind"]][[1]],
  brm_mods_list[["brm_mods_2b_p4"]][[11]],
  
  
  brm_mods_list[["brm_mods_3b_s4"]][[1]],
  brm_mods_list[["brm_mods_3b_s4"]][[6]],
  brm_mods_list[["brm_mods_3b_s4_ind"]][[1]],
  brm_mods_list[["brm_mods_3b_s4"]][[11]], 

  brm_mods_list[["brm_mods_3b_p5"]][[4]],
  brm_mods_list[["brm_mods_3b_p5"]][[8]],
  brm_mods_list[["brm_mods_3b_p5_ind"]][[1]],
  brm_mods_list[["brm_mods_3b_p5"]][[11]]
  
)

# Make tables and write to file =================================================
mods_for_tabs[1:4] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 5) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 9) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 13) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "IAlpha_modeE2" ~ "Alpha^2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls","r1", "brms_param_ests_scale01.csv"))


mods_for_tabs[5:8] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 5) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 9) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 13) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "IAlpha_modeE2" ~ "Alpha^2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls","r1", "brms_param_ests_scale02b_s2.csv"))

mods_for_tabs[9:12] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 6) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 10) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 14) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "IAlpha_modeE2" ~ "Alpha^2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls","r1", "brms_param_ests_scale02b_p4.csv"))


mods_for_tabs[13:16] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 6) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 10) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 14) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode21" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode22" ~ "Alpha^2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls","r1", "brms_param_ests_scale03b_s4.csv"))

mods_for_tabs[17:20] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 5) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 9) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 13) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode21" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode22" ~ "Alpha^2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls","r1", "brms_param_ests_scale03b_p5.csv"))



## Hypothesis tests ===============================================================

# Specific baselines
brm_mods_list[["brm_mods_3b_s4"]][[1]] |> hypothesis("Intercept < 0")
brm_mods_list[["brm_mods_3b_s4"]][[1]] |> hypothesis("TP_mode > 0")
brm_mods_list[["brm_mods_3b_s4"]][[1]] |> hypothesis("Alpha_mode < 0")
brm_mods_list[["brm_mods_3b_s4"]][[1]] |> hypothesis("TP_mode:Alpha_mode > 0")

brm_mods_list[["brm_mods_3b_s4"]][[6]] |> hypothesis("Intercept > 0")
brm_mods_list[["brm_mods_3b_s4"]][[6]] |> hypothesis("Alpha_mode > 0")
brm_mods_list[["brm_mods_3b_s4"]][[6]] |> hypothesis("IAlpha_modeE2 < 0")

brm_mods_list[["brm_mods_3b_s4_ind"]][[1]] |> hypothesis("Intercept > 0")
brm_mods_list[["brm_mods_3b_s4_ind"]][[1]] |> hypothesis("Alpha_mode > 0")
brm_mods_list[["brm_mods_3b_s4_ind"]][[1]] |> hypothesis("IAlpha_modeE2 < 0")

brm_mods_list[["brm_mods_3b_s4"]][[11]] |> hypothesis("Intercept < 0")
brm_mods_list[["brm_mods_3b_s4"]][[11]] |> hypothesis("Alpha_mode > 0")
brm_mods_list[["brm_mods_3b_s4"]][[11]] |> hypothesis("IAlpha_modeE2 < 0")

# Pooled baselines
brm_mods_list[["brm_mods_3b_p5"]][[4]] |> hypothesis("Intercept < 0")
brm_mods_list[["brm_mods_3b_p5"]][[4]] |> hypothesis("TP_mode > 0")
brm_mods_list[["brm_mods_3b_p5"]][[4]] |> hypothesis("Alpha_mode < 0")

brm_mods_list[["brm_mods_3b_p5"]][[8]] |> hypothesis("Intercept > 0")
brm_mods_list[["brm_mods_3b_p5"]][[8]] |> hypothesis("Alpha_mode > 0")
brm_mods_list[["brm_mods_3b_p5"]][[8]] |> hypothesis("IAlpha_modeE2 < 0")

brm_mods_list[["brm_mods_3b_p5_ind"]][[1]] |> hypothesis("Intercept > 0")
brm_mods_list[["brm_mods_3b_p5_ind"]][[1]] |> hypothesis("Alpha_mode > 0")
brm_mods_list[["brm_mods_3b_p5_ind"]][[1]] |> hypothesis("IAlpha_modeE2 < 0")

brm_mods_list[["brm_mods_3b_p5"]][[11]] |> hypothesis("Intercept < 0")
brm_mods_list[["brm_mods_3b_p5"]][[11]] |> hypothesis("Alpha_mode > 0")
brm_mods_list[["brm_mods_3b_p5"]][[11]] |> hypothesis("IAlpha_modeE2 < 0")






