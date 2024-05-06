# Script to compare models using LOO and Bayes r2 

# Prep =========================================================================

# Load libraries
# source(here::here("R", "00_prep.R"))
library(here)        # file paths
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(brms)        # Bayesian Regression Models using 'Stan'
library(performance) # r2_bayes function

# Load model datasets
load(file = here("out", "data", "reg_mod_data_v3.RData"))

# load model objects
load(file = here("out", "models", "brms", "brm_mods_list_20240328.RData"))
# load(file = here("out", "models", "brms", "brm_mods_list_20240327.RData"))
# load(file = here("out", "models", "brms", "brm_mods_list_v2.RData"))
# load(file = here("out", "models", "brms", "brm_mods_list_asym.RData"))

# Check names of loaded objects
names(brm_mods_list)
names(reg_mod_data_tidy)

# check structure
# brm_mods_list[[1]] # this is list, each is a model object, which is itself a list
brm_mods_list[[1]][[1]] # returns a model object

# test comparison
# loo_compare(
#   loo(brm_mods_list[["brm_mods_3b_s4_ind"]][[1]]),
#   loo(brm_mods_list[["brm_mods_3b_s4_ind"]][[2]]),
#   loo(brm_mods_list[["brm_mods_3b_s4_ind"]][[3]]),
#   loo(brm_mods_list[["brm_mods_3b_s4_ind"]][[4]]),
#   loo(brm_mods_list[["brm_mods_3b_s4_ind"]][[5]])
#   )

# Model comparison =============================================================

# create group names for the different sets of models 
model_grp_names <- c("asym", "couplingTP", "couplingTPInd", "couplingMass")


# Scale 1 --------------------------------------------------

# function to compute model comparison metrics for pooled data (no ranefs)
# at scale 1, there are 3 models per hypothesis to compare (5 for scales 2 and 3)
make_model_sel_tbl <- function(models_sub) {
  
  # object with loo comparison of the 3 models
  comp <- loo_compare(
    loo(models_sub[[1]]),
    loo(models_sub[[2]]),
    loo(models_sub[[3]])
    ) 
  
  # make df of un-simplified summary stats of comparisons (elpd, p_loo, looic)
  comp <- invisible(
    print(comp, simplify = FALSE) |>
    as.data.frame() |> 
    mutate(across(everything(), ~round(., digits = 2)))
    )
  
  # create a table for the model comparison metrics
  model_sel_tab <- data.frame(
    elpd_diff = rep(NA, nrow(comp)),
    elpd      = rep(NA, nrow(comp)),
    p_loo     = rep(NA, nrow(comp)),
    looic     = rep(NA, nrow(comp)),
    r2_loo    = rep(NA, nrow(comp)),
    r2_marg   = rep(NA, nrow(comp)),
    r2_cond   = rep(NA, nrow(comp)),
    row.names = rownames(comp)
  )
  
  # fill in the table with the comparison metrics from loo comparison (elpd, p_loo, looic)
  model_sel_tab$elpd_diff <- paste(comp$elpd_diff, " (", comp$se_diff, ")", sep="")
  model_sel_tab$elpd <- paste(comp$elpd_loo, " (",comp$se_elpd_loo, ")",sep="")
  model_sel_tab$p_loo <- paste(comp$p_loo," (",comp$se_p_loo, ")",sep="")
  model_sel_tab$looic <- paste(comp$looic," (",comp$se_looic, ")",sep="")
  
  # make column with model name and arrange table in order so r2 values match model
  # becuase the loo function arranges in order of best fit
  model_sel_tab <- model_sel_tab |> 
    rownames_to_column() |> 
    arrange(
      match(
        rowname, 
        c("models_sub[[1]]", "models_sub[[2]]", "models_sub[[3]]")), 
      desc(rowname)
      )
  
  # calculate r2 for each model_sub and fill in the table with the values
  suppressWarnings(
  for (i in 1:length(models_sub)){
    r2_marg_cond <- r2_bayes(models_sub[[i]])
    model_sel_tab[i,"r2_cond"] <- paste(round(r2_marg_cond$R2_Bayes, digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),")",sep="")
    model_sel_tab[i,"r2_loo"] <- loo_R2(models_sub[[i]]) %>% round(digits=2)
  }
  )
  
  # rearrange by prediction accuracy (elpd_diff)
  model_sel_tab <- model_sel_tab |> 
    mutate(tmp = elpd_diff) |> 
    separate(elpd_diff, into = "sort", sep =" ", extra = "drop") |> 
    mutate(sort = parse_number(sort)) |> 
    arrange(abs(sort)) |> 
    select(-sort) |> 
    rename(elpd_diff = tmp) |> 
    relocate(elpd_diff, .after = rowname) 
  
  model_sel_tab
}

# test function
# make_model_sel_tbl(brm_mods_list[["brm_mods_1_ind"]][1:3])

# Make a list of pooled model objects by hypothesis tested
mods_scale01 <- list(
  brm_mods_list[["brm_mods_1"]][1:3],      # mass ~ TP x alpha
  brm_mods_list[["brm_mods_1"]][4:6],      # TP ~ alpha2
  brm_mods_list[["brm_mods_1_ind"]][1:3],  # TP ~ alpha2
  brm_mods_list[["brm_mods_1"]][7:9]       # mass ~ alpha2
)

# Do the model comparisons
mod_sel_tab_scale01 <- map(mods_scale01, make_model_sel_tbl)

# Add hypothesis group names to the tables
names(mod_sel_tab_scale01) <- model_grp_names

# Tidy up datasets
mod_sel_tab_scale01 <- mod_sel_tab_scale01 |> 
  modify_if(~ any("rowname" %in% colnames(.x)), ~ rename(.x, model = rowname)) |> 
  modify_at(c("asym"), ~ mutate(.x, model = case_when(
    model=="models_sub[[1]]"~"TP ~ mass * alpha", 
    model=="models_sub[[2]]"~"TP ~ mass + alpha", 
    model=="models_sub[[3]]"~"TP ~ 1"
    ))) |> 
  modify_at(c("couplingTP","couplingTPInd"), ~ mutate(.x, model = case_when(
    model=="models_sub[[1]]"~"TP ~ alpha + alpha^2", 
    model=="models_sub[[2]]"~"TP ~ alpha", 
    model=="models_sub[[3]]"~"TP ~ 1"
    ))) |> 
  modify_at(c("couplingMass"), ~ mutate(.x, model = case_when(
    model=="models_sub[[1]]"~"mass ~ alpha + alpha^2", 
    model=="models_sub[[2]]"~"mass ~ alpha",
    model=="models_sub[[3]]"~"mass ~ 1"
    )))
  

# Scales 2-3 with random effects  --------------------------------------------

# Function to compute model comparison metrics for pooled data (with ranefs)
# at scale 2-3, there are 5 models per hypothesis to compare
make_model_sel_tbl_ranef <- function(models_sub) {
  
  # compare models (epld, p_loo, looic)
  comp <- loo_compare(
    loo(models_sub[[1]]),
    loo(models_sub[[2]]),
    loo(models_sub[[3]]), 
    loo(models_sub[[4]]), 
    loo(models_sub[[5]])
    )
  
  # make df of un-simplified summary stats of comparisons (elpd, p_loo, looic)
  comp <- invisible(
    print(comp, simplify = FALSE) |>
      as.data.frame() |> 
      mutate(across(everything(), ~round(., digits = 2)))
  )
  
  # create a table for the model comparison metrics
  model_sel_tab <- data.frame(
    elpd_diff = rep(NA, nrow(comp)),
    elpd = rep(NA, nrow(comp)),
    p_loo = rep(NA, nrow(comp)),
    looic = rep(NA, nrow(comp)),
    r2_loo = rep(NA, nrow(comp)),
    r2_marg = rep(NA, nrow(comp)),
    r2_cond = rep(NA, nrow(comp)),
    row.names = rownames(comp)
  )
  
  # add values to table
  model_sel_tab$elpd_diff <- paste(comp$elpd_diff, " (",comp$se_diff, ")",sep="")
  model_sel_tab$elpd <- paste(comp$elpd_loo, " (",comp$se_elpd_loo, ")",sep="")
  model_sel_tab$p_loo <- paste(comp$p_loo," (",comp$se_p_loo, ")",sep="")
  model_sel_tab$looic <- paste(comp$looic," (",comp$se_looic, ")",sep="")
  
  # arrange in model order so r2 values match model
  # becuase loo function arranges in order of best fit 
  model_sel_tab <- model_sel_tab |> 
    rownames_to_column() |> 
    arrange(
      match(
        rowname, 
        c("models_sub[[1]]", "models_sub[[2]]", "models_sub[[3]]", "models_sub[[4]]", "models_sub[[5]]")),
      desc(rowname)
      )
  
  suppressWarnings(
  for (i in 1:length(models_sub)){
    r2_marg_cond <- r2_bayes(get("models_sub")[[i]])
    model_sel_tab[i,"r2_marg"] <- paste (round(r2_marg_cond$R2_Bayes_marginal, digits=2)," (", attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),")",sep="")
    model_sel_tab[i,"r2_cond"] <- paste (round(r2_marg_cond$R2_Bayes, digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),")",sep="")
    model_sel_tab[i,"r2_loo"] <- loo_R2(get("models_sub")[[i]]) %>% round(digits=2)
  }
  )
  
  # rearrange by prediction accuracy
  model_sel_tab <- model_sel_tab |> 
    mutate(tmp = elpd_diff) |> 
    separate(elpd_diff, into = "sort", sep =" ", extra = "drop") |> 
    mutate(sort = parse_number(sort)) |> 
    arrange(abs(sort)) |> 
    select(-sort) |> 
    rename(elpd_diff = tmp) |> 
    relocate(elpd_diff, .after = rowname)
  
  model_sel_tab
}

# test function
# make_model_sel_tbl_ranef(brm_mods_list[["brm_mods_2b_s2"]][1:5])

# Make a list of pooled model objects by hypothesis tested

mods_scale02b_s2 <- list(
  brm_mods_list[["brm_mods_2b_s2"]][1:5],      # mass ~ TP x alpha
  brm_mods_list[["brm_mods_2b_s2"]][6:10],     # TP ~ alpha2
  brm_mods_list[["brm_mods_2b_s2_ind"]][1:5],  # TP ~ alpha2 (ind)
  brm_mods_list[["brm_mods_2b_s2"]][11:15]     # mass ~ alpha2
)

mods_scale02b_p4 <- list(
  brm_mods_list[["brm_mods_2b_p4"]][1:5],      # mass ~ TP x alpha
  brm_mods_list[["brm_mods_2b_p4"]][6:10],     # TP ~ alpha2
  brm_mods_list[["brm_mods_2b_p4_ind"]][1:5],  # TP ~ alpha2 (ind)
  brm_mods_list[["brm_mods_2b_p4"]][11:15]     # mass ~ alpha2
)

models_sub_03b_s4 <- list(
  brm_mods_list[["brm_mods_3b_s4"]][1:5],      # mass ~ TP x alpha
  brm_mods_list[["brm_mods_3b_s4"]][6:10],     # TP ~ alpha2
  brm_mods_list[["brm_mods_3b_s4_ind"]][1:5],  # TP ~ alpha2 (ind)
  brm_mods_list[["brm_mods_3b_s4"]][11:15]     # mass ~ alpha2
)

models_sub_03b_p5 <- list(
  brm_mods_list[["brm_mods_3b_p5"]][1:5],      # mass ~ TP x alpha
  brm_mods_list[["brm_mods_3b_p5"]][6:10],     # TP ~ alpha2
  brm_mods_list[["brm_mods_3b_p5_ind"]][1:5],  # TP ~ alpha2 (ind)
  brm_mods_list[["brm_mods_3b_p5"]][11:15]     # mass ~ alpha2
)

# Do the model comparisons
mod_sel_tab_scale02b_p4 <- map(mods_scale02b_p4, make_model_sel_tbl_ranef)
mod_sel_tab_scale02b_s2 <- map(mods_scale02b_s2, make_model_sel_tbl_ranef)
mod_sel_tab_scale03b_p5 <- map(models_sub_03b_p5, make_model_sel_tbl_ranef)
mod_sel_tab_scale03b_s4 <- map(models_sub_03b_s4, make_model_sel_tbl_ranef)

# Name the model groups
names(mod_sel_tab_scale02b_p4) <- model_grp_names
names(mod_sel_tab_scale02b_s2) <- model_grp_names
names(mod_sel_tab_scale03b_p5) <- model_grp_names
names(mod_sel_tab_scale03b_s4) <- model_grp_names

# Tidy up datasets

# function to add labels
add_mod_labels <- function(mod_sel_table){
  mod_sel_table |> 
    modify_if(~ any("rowname" %in% colnames(.x)), ~ rename(.x, model = rowname)) |> 
    modify_at(c("asym"), ~ mutate(.x, model = case_when(
      model=="models_sub[[1]]"~"TP~mass*alpha[a]", 
      model=="models_sub[[2]]"~"TP~mass+alpha[a]",
      model=="models_sub[[3]]"~"TP~mass*alpha[b]", 
      model=="models_sub[[4]]"~"TP~mass+alpha[b]",
      model=="models_sub[[5]]"~"TP~1", 
    ))) |> 
    modify_at(c("couplingTP","couplingTPInd"), ~ mutate(.x, model = case_when(
      model=="models_sub[[1]]"~"TP~alpha+alpha^2[a]",
      model=="models_sub[[2]]"~"TP~alpha[a]",
      model=="models_sub[[3]]"~"TP~alpha+alpha^2[b]",
      model=="models_sub[[4]]"~"TP~alpha[b]",
      model=="models_sub[[5]]"~"TP~1"
    ))) |> 
    modify_at(c("couplingMass"), ~ mutate(.x, model = case_when(
      model=="models_sub[[1]]"~"mass~alpha+alpha^2[a]", 
      model=="models_sub[[2]]"~"mass~alpha[a]",
      model=="models_sub[[3]]"~"mass~alpha+alpha^2[b]", 
      model=="models_sub[[4]]"~"mass~alpha[b]",
      model=="models_sub[[5]]"~"mass~1"
    )))
}

# test
add_mod_labels(mod_sel_tab_scale02b_p4)

# add labels
mod_sel_tab_scale02b_p4 <- add_mod_labels(mod_sel_tab_scale02b_p4)
mod_sel_tab_scale02b_s2 <- add_mod_labels(mod_sel_tab_scale02b_s2)
mod_sel_tab_scale03b_p5 <- add_mod_labels(mod_sel_tab_scale03b_p5)
mod_sel_tab_scale03b_s4 <- add_mod_labels(mod_sel_tab_scale03b_s4)


# Export comparison tables =====================================================

mod_sel_tab_scale01
mod_sel_tab_scale02b_s2
mod_sel_tab_scale02b_p4
mod_sel_tab_scale03b_s4
mod_sel_tab_scale03b_p5

bind_rows(
  mod_sel_tab_scale01[[1]], 
  mod_sel_tab_scale01[[2]],
  mod_sel_tab_scale01[[3]],  # ind
  mod_sel_tab_scale01[[4]]
) |> 
  write_csv(here("out", "tbls", "r1", "model_sel_tbl_scale01.csv"))


bind_rows(
  mod_sel_tab_scale02b_s2[[1]], 
  mod_sel_tab_scale02b_s2[[2]],
  mod_sel_tab_scale02b_s2[[3]],  # ind
  mod_sel_tab_scale02b_s2[[4]]
) |> 
  write_csv(here("out", "tbls", "r1",  "model_sel_tbl_scale02b_s2.csv"))


bind_rows(
  mod_sel_tab_scale02b_p4[[1]], 
  mod_sel_tab_scale02b_p4[[2]],
  mod_sel_tab_scale02b_p4[[3]],  # ind
  mod_sel_tab_scale02b_p4[[4]]
) |> 
  write_csv(here("out", "tbls", "r1",  "model_sel_tbl_scale02b_p4.csv"))


bind_rows(
  mod_sel_tab_scale03b_s4[[1]], 
  mod_sel_tab_scale03b_s4[[2]],
  mod_sel_tab_scale03b_s4[[3]],  #ind
  mod_sel_tab_scale03b_s4[[4]]
) |> 
  write_csv(here("out", "tbls", "r1",  "model_sel_tbl_scale03b_s4.csv"))


bind_rows(
  mod_sel_tab_scale03b_p5[[1]], 
  mod_sel_tab_scale03b_p5[[2]],
  mod_sel_tab_scale03b_p5[[3]],  #ind
  mod_sel_tab_scale03b_p5[[4]]
) |> 
  write_csv(here("out", "tbls", "r1",  "model_sel_tbl_scale03b_p5.csv"))


