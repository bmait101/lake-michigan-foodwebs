load(file = here("out", "models", "brms", "mods_scl1.Rdata"))
load(file = here("out", "models", "brms", "mods_scl1_ind.Rdata"))
load(file = here("out", "models", "brms", "mods_scl2_s2.Rdata"))
load(file = here("out", "models", "brms", "mods_scl2_s2_ind.Rdata"))
load(file = here("out", "models", "brms", "mods_scl2_p4.Rdata"))
load(file = here("out", "models", "brms", "mods_scl2_p4_ind.Rdata"))
load(file = here("out", "models", "brms", "mods_scl3_p5.Rdata"))
load(file = here("out", "models", "brms", "mods_scl3_s4.Rdata"))
load(file = here("out", "models", "brms", "mods_scl3_s4_ind.Rdata"))


m <- brm_mods_3b_p5[[8]]
# m <- brm_mods_1_ind[[1]]

m
plot(m)
pp_check(m, type = "dens_overlay", ndraws=10)
plot(conditional_effects(m,"TP_mode:Alpha_mode",re_formula = NULL), points = TRUE)
plot(conditional_effects(m,re_formula = NULL), points = TRUE)


m <- brm_mods_3b_p5
loo(
  m[[1]], 
  m[[2]], 
  m[[3]],
  m[[4]],
  m[[5]]
)
loo(
  m[[6]], 
  m[[7]], 
  m[[8]],
  m[[9]],
  m[[10]]
  )
loo(
  m[[11]], 
  m[[12]], 
  m[[13]],
  m[[14]],
  m[[15]]
)
loo(m[[1]], 
    m[[2]], 
    m[[3]]
    )
