
# model objects
names(brm_mods_list)
names(brm_mods_list_asym)

# extract params 
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
  print(summary_tab)
}

# test
make_param_tab(brm_mods_list[["brm_mods_1"]][[7]])

# list best fit model objects to loop
mods_for_tabs <- list(
  brm_mods_list_asym[["brm_mods_list_asym"]][[2]],
  brm_mods_list[["brm_mods_1"]][[4]],
  brm_mods_list[["brm_mods_1_ind"]][[1]],
  brm_mods_list[["brm_mods_1"]][[7]],
  
  brm_mods_list_asym[["brm_mods_list_asym"]][[2]],
  brm_mods_list[["brm_mods_2a_s1"]][[6]],
  brm_mods_list[["brm_mods_2a_ind"]][[1]],
  brm_mods_list[["brm_mods_2a_s1"]][[11]],
  
  brm_mods_list_asym[["brm_mods_list_asym"]][[2]],
  brm_mods_list[["brm_mods_2b_s2"]][[6]],
  brm_mods_list[["brm_mods_2b_ind"]][[1]],
  brm_mods_list[["brm_mods_2b_s2"]][[11]],
  
  brm_mods_list_asym[["brm_mods_list_asym"]][[1]],
  brm_mods_list[["brm_mods_3a_s3"]][[6]],
  brm_mods_list[["brm_mods_3a_s3"]][[1]],
  brm_mods_list[["brm_mods_3a_s3"]][[11]],
  
  brm_mods_list_asym[["brm_mods_list_asym"]][[1]],
  brm_mods_list[["brm_mods_3b_s4"]][[6]],
  brm_mods_list[["brm_mods_3b_s4"]][[1]],
  brm_mods_list[["brm_mods_3b_s4"]][[11]]
)

mods_for_tabs[1:4] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 5) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 9) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 13) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode21" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode22" ~ "Alpha2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls", "brms_params_scale01.csv"))


mods_for_tabs[5:8] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 5) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 9) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 13) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode21" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode22" ~ "Alpha2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls", "brms_params_scale02a.csv"))

mods_for_tabs[9:12] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 5) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 9) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 13) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode21" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode22" ~ "Alpha2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls", "brms_params_scale02b.csv"))


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
    `Fixed Parameter` == "polyAlpha_mode22" ~ "Alpha2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls", "brms_params_scale03a.csv"))

mods_for_tabs[17:20] |>
  map_df(make_param_tab) |> 
  add_row(`Fixed Parameter` = "Asymmetric TP-body size relationship", .before = 1) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP", .before = 6) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - TP (individual level)", .before = 10) |> 
  add_row(`Fixed Parameter` = "Coupling of different energy pathways - Size", .before = 14) |> 
  mutate(`Fixed Parameter` = case_when(
    `Fixed Parameter` == "log_mass" ~ "Body mass", 
    `Fixed Parameter` == "Alpha_mode" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode21" ~ "Alpha", 
    `Fixed Parameter` == "polyAlpha_mode22" ~ "Alpha2", 
    TRUE ~ `Fixed Parameter`
  )) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, " "))) |> 
  write_csv(here("out", "tbls", "brms_params_scale03b.csv"))


