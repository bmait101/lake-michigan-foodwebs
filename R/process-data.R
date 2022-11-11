
# 1) Process data: duplicates, missing data, outliers
# 2) Combine datasets


# skimr::skim(data)


## Data processing

# ### UWM 2010  ------------------------------------------------------------------
# 
# df_uwm_2010_fish <- df_uwm_2010_fish |> 
#   filter(cn>=1)
# 
# ### CSMI --------------------------------------------------------------------
# 
# ### Missing data
# # df_csmi_2015 <- df_csmi_2015 |> 
# #   drop_na(d15N, d13C, spp_code, depth, site_code, season) |> 
# #   filter(! site %in% c("0", "Z.unk", "MID", "LARS")) |>
# #   filter(! season %in% c("NR")) |>
# #   droplevels()  
# 
# ### Duplicates 
# # dups_csmi <- df_csmi_2015 |> 
# #   janitor::get_dupes(sample_id, spp_code, site, season, depth)
# # dups_csmi |>  View()
# # cannot tell because samples ids not really unique - so not removing any
# 
# df_csmi_2015 <- df_csmi_2015 |> 
#   filter(d13c > -50) |> 
#   filter(sample_id != "5720") |> 
#   filter(!(sample_id == "5688" & common_name == "POM")) |> 
#   filter(depth_m %in% c("2", "18", "46", "91", "110")) |>
#   mutate(depth_m = ifelse(depth_m=="91","110",depth_m)) |>
#   mutate(depth_m = as.numeric(depth_m))
# 
# 
# ### Roth --------------------------------------------------------------------
# 
# ## Duplicates samples
# # df_roth |> janitor::get_dupes(sample_id)  # n=66
# 
# df_roth_2019_fish <- df_roth_2019_fish |>
#   group_by(sample_id) |>
#   slice_head(n = 1) |>
#   ungroup()
# 
# # Drop missing data
# # df_roth <- df_roth |>
# #   drop_na(d15N, d13C, lake_region) |>
# #   droplevels()
# 
# # Remove Lake Huron data, ETC.
# df_roth_2019_fish <- df_roth_2019_fish |>
#   filter(! lake_region == "Huron") |>
#   filter(! common_name %in% c("invertebrate")) |>
#   droplevels()
# 


## Combine observation =========================================================

# head(df_csmi)
# head(df_roth)
# head(df_boot2002)
# head(df_boot2010)

data <- bind_rows(
  df_uwm_2002_fish, 
  df_uwm_2010_fish, 
  df_uwm_2010_benthic, 
  df_nps_2015_salmonids,
  df_glft_2016_fish,
  df_csmi_2015,
  df_roth_2019_fish,
  ) |> 
  select(-taxon_code) |> 
  mutate(
    dataset = factor(
      dataset, levels = c(
        "uwm_2002_2003",
        "uwm_2010_2011",
        "csmi_2015",
        "nps_2015",
        "glft_2016",
        "roth_2019"
        )
      ), 
    compartment = factor(
      compartment, levels = c(
        "pom", 
        "macro-alga",
        "benthic inverts",
        "zooplankton", 
        "fishes"
        )
      ), 
    lake_region = factor(lake_region, levels = c("nw", "ne", "sw", "se"))
    ) |> 
  mutate(across(is.character, as.factor))


## Lipid normalization =========================================================


data <- data |>
  mutate(
    d13c_norm = case_when(
      compartment %in% c("fishes") & cn > 3.5 ~ 
        d13c + (3.5 * (3.5 - cn)) / cn,
      TRUE ~ d13c), 
    d13c_norm2 = if_else(
      compartment == "fishes" & cn > 3.5,
      d13c - 3.32 + (0.99 * cn),
      d13c), 
    d13c_norm3 = if_else(
      compartment %in% c("fishes", "benthic inverts", "zooplankton") & cn > 3.5,
      d13c - 3.32 + (0.99 * cn),
      d13c)
    )


## Filters  =========================================================

data <- data |> 
  filter(lake_region != "Huron") |> 
  filter(d13c < -10 & d13c > -40) |> 
  filter(d13c_norm < -10 & d13c > -40) |>
  filter(d13c_norm2 < -10 & d13c_norm2 > -40) 


