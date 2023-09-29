


reg_mod_data_tidy$p5
reg_mod_data_tidy$s4


tmp <- reg_mod_data_tidy$p4 |> 
  left_join(reg_mod_data_tidy$s2, by = c("species", "lake_region")) |> 
  drop_na(TP_mode.y)

plot(tmp$TP_mode.x, tmp$TP_mode.y)
cor.test(tmp$TP_mode.x, tmp$TP_mode.y)

plot(tmp$Alpha_mode.x, tmp$Alpha_mode.y)
cor.test(tmp$Alpha_mode.x, tmp$Alpha_mode.y)


tmp <- reg_mod_data_tidy$p5 |> 
  left_join(reg_mod_data_tidy$s4, by = c("species", "lake_region", "season")) |> 
  drop_na(TP_mode.y)

plot(tmp$TP_mode.x, tmp$TP_mode.y)
cor.test(tmp$TP_mode.x, tmp$TP_mode.y)

plot(tmp$Alpha_mode.x, tmp$Alpha_mode.y)
cor.test(tmp$Alpha_mode.x, tmp$Alpha_mode.y)

