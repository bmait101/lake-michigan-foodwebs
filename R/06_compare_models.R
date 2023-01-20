
# Prep ====================

source(here::here("R", "00_prep.R"))

# Model objects
load(file = here("out", "models", "brms", "brm_mods_2015.RData"))
# Data for plotting
load(file = here("out", "data", "reg_mod_data_2015.RData"))

names(brm_mods_2015)
names(reg_mod_data_2015)

brm_mods_2015[[1]] # this is list each is a model object, which is a list
str(brm_mods_2015[[1]][[1]]) # returns the model object, which is a list


# Model comparison 2015 =================

# compare the different models using different metrics elpd, looic, r2
# List 5 and 8 are the 'b' analyses (pooled baselines, estimated by grouping)


# Scale 1 --------------------------------------------------

make_model_sel_tbl <- function(models_sub) {
  
  comp <- loo_compare(loo(models_sub[[1]]),loo(models_sub[[2]]),loo(models_sub[[3]]))
  comp_summary <- print(comp, simplify = FALSE, digits = 3) |> as.data.frame()
  
  model_sel_tab <- data.frame(
    elpd_diff = rep(NA, nrow(comp_summary)),
    elpd = rep(NA, nrow(comp_summary)),
    p_loo = rep(NA, nrow(comp_summary)),
    looic = rep(NA, nrow(comp_summary)),
    r2_loo = rep(NA, nrow(comp_summary)),
    r2_marg = rep(NA, nrow(comp_summary)),
    r2_cond = rep(NA, nrow(comp_summary)),
    row.names = rownames(comp_summary)
  )
  
  model_sel_tab$elpd_diff <- paste (comp_summary$elpd_diff %>% round(digits=2), " (",comp_summary$se_diff %>% round(digits=2), ")",sep="")
  model_sel_tab$elpd <- paste (comp_summary$elpd_loo %>% round(digits=2), " (",comp_summary$se_elpd_loo %>% round(digits=2), ")",sep="")
  model_sel_tab$p_loo <- paste (comp_summary$p_loo %>% round(digits=2)," (",comp_summary$se_p_loo %>% round(digits=2), ")",sep="")
  model_sel_tab$looic <- paste (comp_summary$looic %>% round(digits=2)," (",comp_summary$se_looic %>% round(digits=2), ")",sep="")
  
  # arrange in model order so r2 values match model
  model_sel_tab <- model_sel_tab |> 
    rownames_to_column() |> 
    arrange(match(rowname, c("models_sub[[1]]", "models_sub[[2]]", "models_sub[[3]]")), desc(rowname))
  
  for (i in 1:length(models_sub)){
    r2_marg_cond <- r2_bayes(models_sub[[i]])
    model_sel_tab[i,"r2_loo"] <- loo_R2(models_sub[[i]]) %>% round(digits=2)
    # model_sel_tab[i,"r2_marg"] <- paste (r2_marg_cond$R2_Bayes_marginal %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),")",sep="")
    model_sel_tab[i,"r2_cond"] <- paste (r2_marg_cond$R2_Bayes %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),")",sep="")
  }
  
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

models_sub <- list(
  brm_mods_2015[["scale01"]][1:3],
  brm_mods_2015[["scale01"]][4:6],
  brm_mods_2015[["scale01"]][7:9],
  brm_mods_2015[["scale01_ind"]]
)

mod_sel_tab_2015_scale01 <- models_sub |> map(make_model_sel_tbl)


# Scales 2-4 with random effect  --------------------------------------------

make_model_sel_tbl_ranef <- function(models_sub) {
  
  comp <- loo_compare(loo(models_sub[[1]]),loo(models_sub[[2]]),loo(models_sub[[3]]), loo(models_sub[[4]]), loo(models_sub[[5]]))
  comp_summary <- print(comp, simplify = FALSE, digits = 3) |> as.data.frame()
  
  model_sel_tab <- data.frame(
    elpd_diff = rep(NA, nrow(comp_summary)),
    elpd = rep(NA, nrow(comp_summary)),
    p_loo = rep(NA, nrow(comp_summary)),
    looic = rep(NA, nrow(comp_summary)),
    r2_loo = rep(NA, nrow(comp_summary)),
    r2_marg = rep(NA, nrow(comp_summary)),
    r2_cond = rep(NA, nrow(comp_summary)),
    row.names = rownames(comp_summary)
  )
  
  model_sel_tab$elpd_diff <- paste (comp_summary$elpd_diff %>% round(digits=2), " (",comp_summary$se_diff %>% round(digits=2), ")",sep="")
  model_sel_tab$elpd <- paste (comp_summary$elpd_loo %>% round(digits=2), " (",comp_summary$se_elpd_loo %>% round(digits=2), ")",sep="")
  model_sel_tab$p_loo <- paste (comp_summary$p_loo %>% round(digits=2)," (",comp_summary$se_p_loo %>% round(digits=2), ")",sep="")
  model_sel_tab$looic <- paste (comp_summary$looic %>% round(digits=2)," (",comp_summary$se_looic %>% round(digits=2), ")",sep="")
  
  # arrange in model order so r2 values match model
  model_sel_tab <- model_sel_tab |> 
    rownames_to_column() |> 
    arrange(match(rowname, c("models_sub[[1]]", "models_sub[[2]]", "models_sub[[3]]", "models_sub[[4]]", "models_sub[[5]]")), desc(rowname))
  
  for (i in 1:length(models_sub)){
    r2_marg_cond <- r2_bayes(get("models_sub")[[i]])
    model_sel_tab[i,"r2_loo"] <- loo_R2(get("models_sub")[[i]]) %>% round(digits=2)
    model_sel_tab[i,"r2_marg"] <- paste (r2_marg_cond$R2_Bayes_marginal %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes_marginal %>% round(digits=2),")",sep="")
    model_sel_tab[i,"r2_cond"] <- paste (r2_marg_cond$R2_Bayes %>% round(digits=2)," (",attr(r2_marg_cond,"SE")$R2_Bayes %>% round(digits=2),")",sep="")
  }
  
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


models_sub_02 <- list(
  brm_mods_2015[["scale02a"]][1:5],
  brm_mods_2015[["scale02a"]][6:10],
  brm_mods_2015[["scale02a"]][11:15],
  brm_mods_2015[["scale02a_ind"]]
)

models_sub_02b <- list(
  brm_mods_2015[["scale02b"]][1:5],
  brm_mods_2015[["scale02b"]][6:10],
  brm_mods_2015[["scale02b"]][11:15],
  brm_mods_2015[["scale02b_ind"]]
)

models_sub_03 <- list(
  brm_mods_2015[["scale03a"]][1:5],
  brm_mods_2015[["scale03a"]][6:10],
  brm_mods_2015[["scale03a"]][11:15],
  brm_mods_2015[["scale03a_ind"]]
)

models_sub_03b <- list(
  brm_mods_2015[["scale03b"]][1:5],
  brm_mods_2015[["scale03b"]][6:10],
  brm_mods_2015[["scale03b"]][11:15],
  brm_mods_2015[["scale03b_ind"]]
)


mod_sel_tab_2015_scale02 <- models_sub_02 |> map(make_model_sel_tbl_ranef)
# mod_sel_tab_2015_scale02b <- models_sub_02b |> map(make_model_sel_tbl_ranef)
mod_sel_tab_2015_scale03 <- models_sub_03 |> map(make_model_sel_tbl_ranef)
# mod_sel_tab_2015_scale03b <- models_sub_03b |> map(make_model_sel_tbl_ranef)


# Export tables =======================================================

bind_rows(
  mod_sel_tab_2015_scale01[[1]], 
  mod_sel_tab_2015_scale01[[2]],
  mod_sel_tab_2015_scale01[[4]],  #ind
  mod_sel_tab_2015_scale01[[3]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_2015_scale01.csv"))

bind_rows(
  mod_sel_tab_2015_scale02[[1]], 
  mod_sel_tab_2015_scale02[[2]],
  mod_sel_tab_2015_scale02[[4]],  #ind
  mod_sel_tab_2015_scale02[[3]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_2015_scale02.csv"))

# bind_rows(
#   mod_sel_tab_2015_scale02b[[1]], 
#   mod_sel_tab_2015_scale02b[[2]],
#   mod_sel_tab_2015_scale02b[[4]],  #ind
#   mod_sel_tab_2015_scale02b[[3]]
# ) |> 
#   write_csv(here("out", "tbls", "model_sel_tbl_2015_scale02b.csv"))

bind_rows(
  mod_sel_tab_2015_scale03[[1]], 
  mod_sel_tab_2015_scale03[[2]],
  mod_sel_tab_2015_scale03[[4]],  #ind
  mod_sel_tab_2015_scale03[[3]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_2015_scale03.csv"))

# bind_rows(
#   mod_sel_tab_2015_scale03b[[1]], 
#   mod_sel_tab_2015_scale03b[[2]],
#   mod_sel_tab_2015_scale03b[[4]],  #ind
#   mod_sel_tab_2015_scale03b[[3]]
# ) |> 
#   write_csv(here("out", "tbls", "model_sel_tbl_2015_scale03b.csv"))

