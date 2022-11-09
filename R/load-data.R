# Loan and clean datasets

## Prep ========================================================================

# Libraries
pacman::p_load(here, tidyverse, readxl, janitor)

# Source functions
source(here("R", "fx_clean.R"))

# Xref table for CSMI species codes
xref_spp <- 
  read.csv(here("data", "xref-spp.csv")) |> 
  mutate_if(is.character, str_to_lower)



## UWM 2002 fish ===============================================================

raw_uwm_2002_fish <- read_xlsx(
  here("data","raw_turschak_maitland_query.xlsx"), 
  sheet = "2002-2003 SIA Data"
)

df_uwm_2002_fish <- raw_uwm_2002_fish |> 
  cleans_names_and_caps() |> 
  select(
    sample_id,
    date, 
    site_code = port_location, 
    depth_m, 
    common_name = species, 
    length_mm = tl,
    d15n, 
    d13c, 
    ug_n = n_ug, 
    ug_c = c_ug
  ) 

df_uwm_2002_fish <- df_uwm_2002_fish |> 
  mutate(
    dataset = "uwm_2002_2003",
    weight_g = NA, 
    taxon_group = NA, 
    sample_type = case_when(
      common_name %in% c("Quagga", "Zebra", "Zooplankton") ~ "invert", 
      TRUE ~ "fish"
    ),
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    lake_region = case_when(
      site_code %in% c(
        "north manitou", "northeast reef", "pyramid point", "south manitou 2") ~ "NE", 
      site_code %in% c("door county") ~ "NW", 
      site_code %in% c("s1", "s2", "sheboygan reef") ~ "SE", 
      site_code %in% c(
        "atwater", "east reef", "east shore of l michigan", 
        "east shored of l michigan", "fox point", "fox point offshore", 
        "linnwood", "milwaukee offshore", "milwaukee south", "port washington", 
        "s3", "s4", "sheboygan nearshore", "sheboygan offshore", 
        "whitefish bay", "wind point") ~ "SW" 
    ), 
    cn = ug_c / ug_n
  ) |>
  select(-ug_c, -ug_n) |> 
  mutate(
    season = case_when(
      season == "1" ~ "spring", 
      season == "2" ~ "summer", 
      season == "3" ~ "fall", 
      season == "4" ~ "winter",
      TRUE ~ season)) |> 
  relocate(dataset, .before = sample_id) |> 
  relocate(c(year, season), .after = date) |> 
  relocate(lake_region, .after = season) |> 
  relocate(sample_type, .after = depth_m) |> 
  relocate(cn, .after = d13c) |> 
  relocate(weight_g, .after = length_mm) |> 
  relocate(taxon_group, .after = common_name)


## UWM 2010 fish ===============================================================

raw_uwm_2010_fish <- read_xlsx(
  here("data","raw_bootsma_2010_2011_fish.xlsx"), 
  sheet = "FishSIResults2010_2011"
)

df_uwm_2010_fish <- raw_uwm_2010_fish |> 
  cleans_names_and_caps() |> 
  select(
    sample_id = sample_num,
    date, 
    site_code = site_name, 
    depth_m = site_depth_general, 
    common_name = fish_species, 
    length_mm = fish_tl,
    d15n = corrected_n_d_29_28, 
    d13c = corrected_c_d_13_12, 
    ug_n, 
    ug_c
  ) 

df_uwm_2010_fish <- df_uwm_2010_fish |> 
  mutate(
    dataset = "uwm_2010_2011",
    sample_type = "fish", 
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    taxon_group = NA, 
    weight_g = NA, 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    depth_m = case_when(
      depth_m == "shallow" ~ 3, 
      depth_m == "intermediate" ~ 8, 
      depth_m == "deep" ~ 15, 
      depth_m == "various" ~ 8
    ),
    lake_region = case_when(
      site_code %in% c("6r") ~ "NE", 
      site_code %in% c("5r") ~ "NW", 
      site_code %in% c("1s", "1r", "2s", "2r", "3r") ~ "SE", 
      site_code %in% c("3s", "4s") ~ "SW" ), 
    cn = ug_c / ug_n
  ) |>
  select(-ug_c, -ug_n) |> 
  mutate(
    season = case_when(
      season == "1" ~ "spring", 
      season == "2" ~ "summer", 
      season == "3" ~ "fall", 
      TRUE ~ season)) |> 
  mutate(year = case_when(
    sample_id == "10micitydnr" ~ 2010, 
    TRUE ~ year
  )) |> 
  relocate(dataset, .before = sample_id) |> 
  relocate(c(year, season), .after = date) |> 
  relocate(lake_region, .after = season) |> 
  relocate(sample_type, .after = depth_m) |> 
  relocate(cn, .after = d13c) |> 
  relocate(weight_g, .after = length_mm) |> 
  relocate(taxon_group, .after = common_name)


## UWM 2010 baselines ==========================================================

# path <- here("data", "raw_bootsma_2010_baselines.xlsx") 
# 
# path %>% 
#   excel_sheets() %>% 
#   set_names() %>% 
#   map(read_excel, path = path)


## NPS 2015 salmonids ==========================================================

raw_nps_2015_salmonids <- read_csv(here("data", "raw_Salmonine_Isotope_2015.csv"))

df_nps_2015_salmonids <- raw_nps_2015_salmonids |> 
  cleans_names_and_caps() |> 
  select(
    sample_id = id,
    date = collection_date, 
    lake_region = quadrant, 
    site_code = port_where_fish_was_landed, 
    common_name = species, 
    length_mm = tl_mm,
    weight_kg = wt_kg,
    d15n = d15n_14n, 
    d13c = d13c_12c, 
    cn
  ) 

df_nps_2015_salmonids <- df_nps_2015_salmonids |> 
  mutate(
    dataset = "nps_2015",
    sample_id = as.character(sample_id),
    sample_type = "fish", 
    date = lubridate::mdy(date),
    year = lubridate::year(date), 
    depth_m = NA,
    taxon_group = NA, 
    weight_g = weight_kg * 1000, 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    lake_region = case_when(
      lake_region == "northeast" ~ "NE", 
      lake_region == "northwest" ~ "NW", 
      lake_region == "southeast" ~ "SE", 
      lake_region == "southwest" ~ "SW" 
      ), 
    common_name = case_when(
      common_name == "bnt" ~ "brown trout",
      common_name == "chs" ~ "chinook salmon",
      common_name == "cos" ~ "coho salmon",
      common_name == "lat" ~ "lake trout",
      common_name == "rbt" ~ "rainbow trout"
      )
    ) |>
  select(-weight_kg) |> 
  mutate(
    season = case_when(
      season == "1" ~ "spring", 
      season == "2" ~ "summer", 
      season == "3" ~ "fall", 
      season == "4" ~ "winter", 
      TRUE ~ season)) |> 
  relocate(dataset, .before = sample_id) |> 
  relocate(c(year, season), .after = date) |> 
  relocate(lake_region, .after = season) |> 
  relocate(sample_type, .after = depth_m) |> 
  relocate(cn, .after = d13c) |> 
  relocate(weight_g, .after = length_mm) |> 
  relocate(taxon_group, .after = common_name)



## GLFT 2016 salmonids =========================================================

raw_glft_2016_fish <- read_xlsx(
  here("data","raw_turschak_maitland_query.xlsx"), 
  sheet = "2016_GLFT_Data_Query"
)

df_glft_2016_fish <- raw_glft_2016_fish |> 
  cleans_names_and_caps() |> 
  select(
    sample_id,
    date, 
    site_code = port_location, 
    depth_m, 
    common_name = species, 
    length_mm = tl,
    d15n, 
    d13c, 
    ug_n = n_ug, 
    ug_c = c_ug
  ) 

df_glft_2016_fish <- df_glft_2016_fish |> 
  mutate(
    dataset = "glft_2016",
    weight_g = NA, 
    taxon_group = NA, 
    sample_type = "fish",
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    lake_region = case_when(
      site_code %in% c(
        "arcadia", "charlevoix", "elk rapids", "frankfort", "little traverse bay",
        "ludington", "manistee"
        ) ~ "NE", 
      site_code %in% c(
        "algoma", "cardy's reef", "clay banks", "kewaunee", "manitowoc",
        "strugeon bay", "sturgeon bay", "two rivers"
        ) ~ "NW", 
      site_code %in% c(
        "holland", "michigan city", "muskegon", "south haven", "st joseph"
        ) ~ "SE", 
      site_code %in% c(
        "east chicago", "burns harbor", "grand haven", "kenosha", "milwaukee", 
        "port washington", "racine", "sheboygan", "waukegan"
      ) ~ "SW", 
      TRUE ~ NA_character_
    ), 
    cn = ug_c / ug_n
  ) |>
  select(-ug_c, -ug_n) |> 
  mutate(depth_m = as.numeric(depth_m)) |> 
  mutate(
    season = case_when(
      season == "1" ~ "spring", 
      season == "2" ~ "summer", 
      season == "3" ~ "fall", 
      season == "4" ~ "winter",
      TRUE ~ season)) |> 
  relocate(dataset, .before = sample_id) |> 
  relocate(c(year, season), .after = date) |> 
  relocate(lake_region, .after = season) |> 
  relocate(sample_type, .after = depth_m) |> 
  relocate(cn, .after = d13c) |> 
  relocate(weight_g, .after = length_mm) |> 
  relocate(taxon_group, .after = common_name)



## CSMI 2015 ===================================================================

raw_csmi_2015 <- read_xlsx(
  here("data","raw-CSMI-2015.xlsx"), 
  sheet = "Combined UF MED"
  )

df_csmi_2015 <- raw_csmi_2015 |> 
  cleans_names_and_caps() |> 
  select(
    sample_id, 
    season,
    site_code = site,
    depth_m,
    sample_type = f_i,
    spp_code = species_o, 
    d15n, 
    d13c = d13cb,
    cn = c_n
    )

df_csmi_2015 <- df_csmi_2015 |> 
  mutate(
    dataset = "csmi_2015",
    length_mm = NA, 
    weight_g = NA, 
    date = NA,
    year = 2015,  
    lake_region = case_when(
      site_code %in% c("arc", "luc") ~ "NE", 
      site_code %in% c("sau", "stj") ~ "SE", 
      site_code %in% c("stb", "man") ~ "NW", 
      site_code %in% c("rac", "wak") ~ "SW"
      ),
    spp_code = ifelse(spp_code=="LTR", "LAT", spp_code),
    sample_type = case_when(
      sample_type == "f" ~ "fish", 
      sample_type == "i" ~ "invert", 
      TRUE ~ NA_character_
      ), 
    season = case_when(
      season == "1" ~ "spring", 
      season == "2" ~ "summer", 
      season == "3" ~ "fall", 
      season == "4" ~ "winter",
      TRUE ~ season)
    ) |> 
  mutate(sample_type = coalesce(sample_type, "POM")) |>
  mutate(sample_type = case_when(
    spp_code == "alg" ~ "algae", 
    TRUE ~ sample_type
  )) |> 
  left_join(xref_spp, by = "spp_code") |> 
  select(-spp_code) |> 
  relocate(dataset, .before = sample_id) |>
  relocate(c(common_name, taxon_group), .after = sample_type) |>
  relocate(c(date, year), .before = season) |>
  relocate(c(lake_region), .before = site_code) |> 
  relocate(c(length_mm, weight_g), .before = d15n)



## Roth 2019 ===================================================================

raw_roth_2019_fish <- readr::read_csv(here("data", "raw-Roth-2019.csv"))

df_roth_2019_fish <- raw_roth_2019_fish |>
  cleans_names_and_caps() |> 
  select(
    sample_id = msu_id, 
    date, 
    site_code = cap_site, 
    depth_m,   
    lake_region = region,
    spp_code = species,
    d15n = del15n,  
    d13c = del13c, 
    cn = c_n_ratio,
    length_mm = t_lmm, 
    weight_g = t_wg
    )

df_roth_2019_fish <- df_roth_2019_fish |> 
  mutate(
    dataset = "roth_2019",
    sample_type = case_when(
      spp_code %in% c("dre", "int") ~ "invert", 
      TRUE ~ "fish"), 
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1))
  ) |> 
  mutate(
    year = case_when(
      is.na(year) ~ 2019, 
      TRUE ~ 2019
    ),
    lake_region = case_when(
      lake_region %in% c("nem") ~ "NE", 
      lake_region %in% c("sem") ~ "SE", 
      lake_region %in% c("nwm") ~ "NW", 
      lake_region %in% c("swm") ~ "SW", 
      TRUE ~ "Huron"
    ),
    season = case_when(
        season == "1" ~ "spring", 
        season == "2" ~ "summer", 
        season == "3" ~ "fall", 
        season == "4" ~ "winter",
        TRUE ~ season)
    ) |> 
  relocate(c(year, season), .after = date) 

df_roth_2019_fish <- df_roth_2019_fish |> 
  mutate(
    spp_code = case_when(
      spp_code=="dre" ~ "mus", 
      spp_code=="smt" ~ "ras", 
      spp_code=="rgb" ~ "rog", 
      TRUE ~ spp_code)) |> 
  left_join(xref_spp, by = "spp_code") |> 
  select(-spp_code) |> 
  relocate(dataset, .before = sample_id) |>
  relocate(lake_region, .before = site_code) |> 
  relocate(c(sample_type, common_name, taxon_group), .after = depth_m) |> 
  relocate(c(d15n, d13c, cn, length_mm, weight_g), .after = taxon_group) 








