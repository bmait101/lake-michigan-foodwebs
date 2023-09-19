
# Prep =========================================================================

source(here::here("R", "00_prep.R"))

# Data

load(file = here("out", "data", "reg_mod_data_v3.RData"))

# Model objects

load(file = here("out", "models", "brms", "brm_mods_list_v2.RData"))
load(file = here("out", "models", "brms", "brm_mods_list_asym.RData"))

# Check 
names(brm_mods_list)
names(brm_mods_list_asym)
names(reg_mod_data_tidy)

brm_mods_list[[1]] # this is list, each is a model object, which is itself a list
str(brm_mods_list[[1]][[1]]) # returns a model object

# plot(brm_mods_list[[1]][[1]])


# Model comparison =================
# compare the different models using different metrics elpd, looic, r2

# names for model groups
model_grp_names <- c("asym", "couplingTP", "couplingTPInd", "couplingMass")
# model_grp_names_noInd <- c("asym", "couplingTP", "couplingMass")

# loo_compare(loo(brm_mods_1[[4]]),loo(brm_mods_1[[5]]),loo(brm_mods_1[[6]]))

# Scale 1 --------------------------------------------------

# Function to compute model comparison metrics for pooled data (no ranefs)
make_model_sel_tbl <- function(models_sub) {
  
  comp <- loo_compare(loo(models_sub[[1]]),loo(models_sub[[2]]),loo(models_sub[[3]]))  # three models to compare
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

# test it
make_model_sel_tbl(brm_mods_list_asym[["brm_mods_1_asym"]][1:3])

# Make a list of pooled model objects by hypothesis tested

mods_scale01 <- list(
  brm_mods_list_asym[["brm_mods_1_asym"]][1:3],  # mass ~ TP x alpha
  brm_mods_list[["brm_mods_1"]][4:6],  # TP ~ alpha
  brm_mods_list[["brm_mods_1_ind"]][1:3],  # TP ~ alpha
  brm_mods_list[["brm_mods_1"]][7:9]  # mass ~ alpha
)

# Do the model comparisons

mod_sel_tab_scale01 <- mods_scale01 |> map(make_model_sel_tbl)
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
  

# Scales 2-4 with random effect  --------------------------------------------

# Function to compute model comparison metrics for pooled data (with ranefs)
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

# Make a list of pooled model objects by hypothesis tested

# mods_scale02a_s1 <- list(
#   brm_mods_list_asym[["brm_mods_2a_s1_asym"]][1:5],
#   brm_mods_list[["brm_mods_2a_s1"]][6:10],
#   brm_mods_list[["brm_mods_2a_s1_ind"]][1:5],
#   brm_mods_list[["brm_mods_2a_s1"]][11:15]
# )

# mods_scale02a_p2 <- list(
#   brm_mods_list_asym[["brm_mods_2a_p2_asym"]][1:5],
#   brm_mods_list[["brm_mods_2a_p2"]][6:10],
#   brm_mods_list[["brm_mods_2a_p2_ind"]][1:5],
#   brm_mods_list[["brm_mods_2a_p2"]][11:15]
# )

mods_scale02b_s2 <- list(
  brm_mods_list_asym[["brm_mods_2b_s2_asym"]][1:5],
  brm_mods_list[["brm_mods_2b_s2"]][6:10],
  brm_mods_list[["brm_mods_2b_s2_ind"]][1:5],
  brm_mods_list[["brm_mods_2b_s2"]][11:15]
)

mods_scale02b_p4 <- list(
  brm_mods_list_asym[["brm_mods_2b_p4_asym"]][1:5],
  brm_mods_list[["brm_mods_2b_p4"]][6:10],
  brm_mods_list[["brm_mods_2b_p4_ind"]][1:5],
  brm_mods_list[["brm_mods_2b_p4"]][11:15]
)

# models_sub_03a_s3 <- list(
#   brm_mods_list_asym[["brm_mods_3a_s3_asym"]][1:5],
#   brm_mods_list[["brm_mods_3a_s3"]][6:10],
#   brm_mods_list[["brm_mods_3a_s3_ind"]][1:5],
#   brm_mods_list[["brm_mods_3a_s3"]][11:15]
# )
# 
# models_sub_03a_p3 <- list(
#   brm_mods_list_asym[["brm_mods_3a_p3_asym"]][1:5],
#   brm_mods_list[["brm_mods_3a_p3"]][6:10],
#   brm_mods_list[["brm_mods_3a_p3_ind"]][1:5],
#   brm_mods_list[["brm_mods_3a_p3"]][11:15]
# )

models_sub_03b_s4 <- list(
  brm_mods_list_asym[["brm_mods_3b_s4_asym"]][1:5],
  brm_mods_list[["brm_mods_3b_s4"]][6:10],
  brm_mods_list[["brm_mods_3b_s4_ind"]][1:5],
  brm_mods_list[["brm_mods_3b_s4"]][11:15]
)

models_sub_03b_p5 <- list(
  brm_mods_list_asym[["brm_mods_3b_p5_asym"]][1:5],
  brm_mods_list[["brm_mods_3b_p5"]][6:10],
  brm_mods_list[["brm_mods_3b_p5_ind"]][1:5],
  brm_mods_list[["brm_mods_3b_p5"]][11:15]
)

# Do the model comparisons

# mod_sel_tab_scale02a_p2 <- mods_scale02a_p2 |> map(make_model_sel_tbl_ranef)
# mod_sel_tab_scale02a_s1 <- mods_scale02a_s1 |> map(make_model_sel_tbl_ranef)
mod_sel_tab_scale02b_p4 <- mods_scale02b_p4 |> map(make_model_sel_tbl_ranef)
mod_sel_tab_scale02b_s2 <- mods_scale02b_s2 |> map(make_model_sel_tbl_ranef)
# mod_sel_tab_scale03a_p3 <- models_sub_03a_p3 |> map(make_model_sel_tbl_ranef)
# mod_sel_tab_scale03a_s3 <- models_sub_03a_s3 |> map(make_model_sel_tbl_ranef)
mod_sel_tab_scale03b_p5 <- models_sub_03b_p5 |> map(make_model_sel_tbl_ranef)
mod_sel_tab_scale03b_s4 <- models_sub_03b_s4 |> map(make_model_sel_tbl_ranef)

# Name the model groups

# names(mod_sel_tab_scale02a_p2) <- model_grp_names
# names(mod_sel_tab_scale02a_s1) <- model_grp_names
names(mod_sel_tab_scale02b_p4) <- model_grp_names
names(mod_sel_tab_scale02b_s2) <- model_grp_names
# names(mod_sel_tab_scale03a_p3) <- model_grp_names
# names(mod_sel_tab_scale03a_s3) <- model_grp_names
names(mod_sel_tab_scale03b_p5) <- model_grp_names
names(mod_sel_tab_scale03b_s4) <- model_grp_names

# Tidy up datasets

# build
# mod_sel_tab_scale02a_s1 |> 
#   modify_if(~ any("rowname" %in% colnames(.x)), ~ rename(.x, model = rowname)) |> 
#   modify_at(c("asym"), ~ mutate(.x, model = case_when(model=="models_sub[[1]]"~"TP~mass*alpha[a]", model=="models_sub[[2]]"~"TP~mass+alpha[a]",model=="models_sub[[3]]"~"TP~mass*alpha[b]", model=="models_sub[[4]]"~"TP~mass+alpha[b]",model=="models_sub[[5]]"~"TP~1", ))) |> 
#   modify_at(c("couplingTP","couplingTPInd"), ~ mutate(.x, model = case_when(model=="models_sub[[1]]"~"TP~alpha+alpha^2[a]",model=="models_sub[[2]]"~"TP~alpha[a]",model=="models_sub[[3]]"~"TP~alpha+alpha^2[b]",model=="models_sub[[4]]"~"TP~alpha[b]",model=="models_sub[[5]]"~"TP~1"))) |> 
#   modify_at(c("couplingMass"), ~ mutate(.x, model = case_when(model=="models_sub[[1]]"~"mass~alpha+alpha^2[a]", model=="models_sub[[2]]"~"mass~alpha[a]",model=="models_sub[[3]]"~"mass~alpha+alpha^2[b]", model=="models_sub[[4]]"~"mass~alpha[b]",model=="models_sub[[5]]"~"mass~1")))

#function to add labels
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
# add_mod_labels(mod_sel_tab_scale02a_p2)
# add_mod_labels(mod_sel_tab_scale02a_s1)

# mod_sel_tab_scale02a_p2 <- add_mod_labels(mod_sel_tab_scale02a_p2)
# mod_sel_tab_scale02a_s1 <- add_mod_labels(mod_sel_tab_scale02a_s1)
mod_sel_tab_scale02b_p4 <- add_mod_labels(mod_sel_tab_scale02b_p4)
mod_sel_tab_scale02b_s2 <- add_mod_labels(mod_sel_tab_scale02b_s2)
# mod_sel_tab_scale03a_p3 <- add_mod_labels(mod_sel_tab_scale03a_p3)
# mod_sel_tab_scale03a_s3 <- add_mod_labels(mod_sel_tab_scale03a_s3)
mod_sel_tab_scale03b_p5 <- add_mod_labels(mod_sel_tab_scale03b_p5)
mod_sel_tab_scale03b_s4 <- add_mod_labels(mod_sel_tab_scale03b_s4)


# Export comparison tables =====================================================



# export
mod_sel_tab_scale01
bind_rows(
  mod_sel_tab_scale01[[1]], 
  mod_sel_tab_scale01[[2]],
  mod_sel_tab_scale01[[3]],
  mod_sel_tab_scale01[[4]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_scale01.csv"))

# bind_rows(
#   mod_sel_tab_scale02a_s1[[1]], 
#   mod_sel_tab_scale02a_s1[[2]],
#   mod_sel_tab_scale02a_s1[[3]],
#   mod_sel_tab_scale02a_s1[[4]]
# ) |> 
#   write_csv(here("out", "tbls", "model_sel_tbl_scale02a_s1.csv"))
# 
# bind_rows(
#   mod_sel_tab_scale02a_p2[[1]], 
#   mod_sel_tab_scale02a_p2[[2]],
#   mod_sel_tab_scale02a_p2[[3]],
#   mod_sel_tab_scale02a_p2[[4]]
# ) |> 
#   write_csv(here("out", "tbls", "model_sel_tbl_scale02a_p2.csv"))

mod_sel_tab_scale02b_s2
bind_rows(
  mod_sel_tab_scale02b_s2[[1]], 
  mod_sel_tab_scale02b_s2[[2]],
  mod_sel_tab_scale02b_s2[[3]],
  mod_sel_tab_scale02b_s2[[4]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_scale02b_s2.csv"))

mod_sel_tab_scale02b_p4
bind_rows(
  mod_sel_tab_scale02b_p4[[1]], 
  mod_sel_tab_scale02b_p4[[2]],
  mod_sel_tab_scale02b_p4[[3]],
  mod_sel_tab_scale02b_p4[[4]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_scale02b_p4.csv"))

# bind_rows(
#   mod_sel_tab_scale03a_s3[[1]], 
#   mod_sel_tab_scale03a_s3[[2]],
#   mod_sel_tab_scale03a_s3[[3]],  #ind
#   mod_sel_tab_scale03a_s3[[4]]
# ) |> 
#   write_csv(here("out", "tbls", "model_sel_tbl_scale03a_s3.csv"))
# 
# bind_rows(
#   mod_sel_tab_scale03a_p3[[1]], 
#   mod_sel_tab_scale03a_p3[[2]],
#   mod_sel_tab_scale03a_p3[[3]],  #ind
#   mod_sel_tab_scale03a_p3[[4]]
# ) |> 
#   write_csv(here("out", "tbls", "model_sel_tbl_scale03a_p3.csv"))

mod_sel_tab_scale03b_s4
bind_rows(
  mod_sel_tab_scale03b_s4[[1]], 
  mod_sel_tab_scale03b_s4[[2]],
  mod_sel_tab_scale03b_s4[[3]],  #ind
  mod_sel_tab_scale03b_s4[[4]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_scale03b_s4.csv"))

mod_sel_tab_scale03b_p5
bind_rows(
  mod_sel_tab_scale03b_p5[[1]], 
  mod_sel_tab_scale03b_p5[[2]],
  mod_sel_tab_scale03b_p5[[3]],  #ind
  mod_sel_tab_scale03b_p5[[4]]
) |> 
  write_csv(here("out", "tbls", "model_sel_tbl_scale03b_p5.csv"))


