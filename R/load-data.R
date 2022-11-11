# Loan and clean datasets

## Prep ========================================================================

# Libraries
pacman::p_load(here, tidyverse, readxl, janitor)

# Source functions and xrefs
source(here("R", "fx_helpers.R"))
source(here("R", "xrefs.R"))


# ----# 

## UWM 2002 fish ===============================================================

(raw_uwm_2002_fish <- 
  read_xlsx(
    here("data","raw_turschak_maitland_query.xlsx"), 
    sheet = "2002-2003 SIA Data") |> 
  cleans_names_and_caps())

# Tidy up and standardize variables
df_uwm_2002_fish <- raw_uwm_2002_fish |> 
  select(
    sample_id,
    date, 
    port = port_location, 
    depth_m, 
    taxon_specific = species, 
    length_mm = tl,
    d15n, d13c, 
    ug_n = n_ug, ug_c = c_ug
  ) |> 
  mutate(cn = ug_c / ug_n) |> 
  select(-ug_c, -ug_n) |> 
  # rename a few ports to match the xref table
  mutate(port = case_when(
    port =="s1" ~ "whitefish bay", port =="s2" ~ "dead river",
    port =="s3" ~ "michigan city", port =="s4" ~ "muskegon",
    TRUE ~ port
  )) |> 
  # join xref tables
  left_join(xref_ports[,3:4], by = "port") |> 
  left_join(xref_compartments, by = "taxon_specific") |> 
  # standardize columns
  mutate(
    dataset = "uwm_2002_2003",
    site_code = NA, 
    mass_g = NA, 
    num_ind = NA,
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1))
    ) |>
  select(-site_code) |>
  relocate_columns()


## UWM 2010 fish ===============================================================

# Read data
(raw_uwm_2010_fish <-
  read_xlsx(
    here("data","raw_bootsma_2010_2011_fish.xlsx"),
    sheet = "FishSIResults2010_2011")|>
  cleans_names_and_caps())

# Tidy up and standardize variables
df_uwm_2010_fish <- raw_uwm_2010_fish |> 
  select(
    sample_id = sample_num,
    date, 
    site_code_happel = site_name, 
    depth_m = site_depth_general, 
    taxon_specific = fish_species, 
    length_mm = fish_tl,
    d15n = corrected_n_d_29_28, 
    d13c = corrected_c_d_13_12, 
    ug_n, ug_c
  ) |> 
  mutate(cn = ug_c / ug_n) |> 
  select(-ug_c, -ug_n)

df_uwm_2010_fish <- df_uwm_2010_fish |> 
  # rename a few ports to match the xref table (assuming we-reef is sheyb reef)
  mutate(
    site_code_happel = case_when(
      site_code_happel == "fpt" ~ "1r", 
      TRUE ~ site_code_happel), 
    taxon_specific = case_when(
      taxon_specific == "9 spine stickleback" ~ "ninespine stickleback", 
      taxon_specific == "3 spine stickleback" ~ "threespine stickleback", 
      taxon_specific == "shorthead river redhorse" ~ "shorthead redhorse", 
      # taxon_specific == "gizzard shad" ~ "gizzard shad", 
      TRUE ~ taxon_specific
      ), 
    ) |> 
  # join xref tables
  left_join(xref_ports[,c(1,3:4)] |> filter(!is.na(site_code_happel)), 
            by = "site_code_happel") |> 
  left_join(xref_compartments, by = "taxon_specific") |> 
  rename(site_code = site_code_happel) |> 
  # standardize columns
  mutate(
    dataset = "uwm_2010_2011",
    num_ind = NA,
    mass_g = NA, 
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    # convert the text depth labels to numeric
    depth_m = case_when(
      depth_m == "shallow" ~ 3, 
      depth_m == "intermediate" ~ 8, 
      depth_m == "deep" ~ 15, 
      depth_m == "various" ~ 8
    )
    ) |>
  mutate(year = case_when(
    sample_id == "10micitydnr" ~ 2010, 
    TRUE ~ year
  )) |> 
  select(-site_code) |> 
  relocate_columns()



## UWM 2010 baselines ==========================================================

# Notes: 
# I filled in missing site data for much easier data cleaning
# I had to add a flag column to the raw data: many isotope values 
    # were stiked out and the strike does not come thru in import, 
    # so my flag will remove them to help data cleaning
# all zoops are 63 um mesh

# Read data
raw_uwm_2010_benthic <- read_xlsx(
  here("data","raw_Nearshore seston and benthos stable isotopes 2010 for Bryan Maitland.xlsx"), 
  sheet = "Benthic", 
  skip = 6
)

# Filter out flagged records and select columns
df_uwm_2010_benthic <- raw_uwm_2010_benthic |> 
  filter(!flag==1) |> select(-flag) |>   # remove flagged rows and flag column
  cleans_names_and_caps() |> 
  select(
    sample_id = sample_number, 
    date = date_collected,
    site, 
    depth_m, 
    taxon_specific = taxa, 
    num_ind = number_of_individuals, 
    mass_g = dry_mass_g, 
    d15n = d_29_28, 
    d13c = d_13_12, 
    ug_c, ug_n
  ) |> 
  mutate(cn = ug_c / ug_n) |>
  select(-ug_c, -ug_n)

# Deal with cluster f*ck of site/port names and stardardize
df_uwm_2010_benthic <- 
  df_uwm_2010_benthic |> 
  separate(site, into = c("port"), extra = "drop") |>
  mutate(port = case_when(
    port %in% c("il", "in", "mi") ~ NA_character_, 
    port == "aw" ~ "fox point",
    port == "wfb" ~ "whitefish bay", 
    port == "stbay" ~ "sturgeon bay",
    TRUE ~ port)
    ) |> 
  separate(
    sample_id, 
    into = c("a", "b", "site_code_happel", "d"), 
    sep = c(2,4, 6),
    extra = "merge", 
    remove = FALSE
    ) |> 
  mutate(
    site_code_happel = case_when(
      a == "4r" ~ "4r", 
      is.na(site_code_happel) & port == "sturgeon bay" ~ "6r", 
      is.na(site_code_happel) & port == "fox point" ~ "1s", 
      is.na(site_code_happel) & port == "whitefish bay" ~ "1s", 
      TRUE ~ site_code_happel)
    ) |> 
  select(-port, -a, -b, -d) |> 
  left_join(xref_ports[,c(1,3:4)], by = "site_code_happel") |> 
  mutate(taxon_specific = case_when(
    taxon_specific %in% c("r1 quagga", "r3 quagga") ~ "quagga", 
    taxon_specific %in% c("oligochaeteb") ~ "oligochaete", 
    taxon_specific %in% c("algae a", "algae b", "algae c") ~ "cladophora", 
    TRUE ~ taxon_specific)
    ) |> 
  rename(site_code = site_code_happel) |> 
  separate(taxon_specific, into = "taxon_specific", extra = "drop") |>
  left_join(xref_compartments, by = "taxon_specific") |> 
  # Standardize 
  mutate(
    dataset = "uwm_2010_2011",
    date = lubridate::as_date(date),
    length_mm = NA, 
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    depth_m = as.numeric(ifelse(depth_m == "integrated", "8", depth_m)),
    mass_g = as.numeric(mass_g),
    num_ind = as.numeric(ifelse(num_ind == "n/a", NA_character_, num_ind))) |>
  select(-site_code) |>
  relocate_columns()



## NPS 2015 salmonids ==========================================================

# Read data
(raw_nps_2015_salmonids <- 
  read_csv(
    here("data", "raw_Salmonine_Isotope_2015.csv")
    ) |> 
  cleans_names_and_caps())

# Tidy up and standardize variables
df_nps_2015_salmonids <-
  raw_nps_2015_salmonids |> 
  select(
    sample_id = id,
    date = collection_date, 
    port = port_where_fish_was_landed, 
    taxon_code = species, 
    length_mm = tl_mm,
    weight_kg = wt_kg,
    d15n = d15n_14n, 
    d13c = d13c_12c, 
    cn
  ) 

# join xref tables
df_nps_2015_salmonids <-
  df_nps_2015_salmonids |> 
  # rename a few ports to match the xref table
  mutate(
    port = case_when(
      port =="milwaukee - south shore park" ~ "milwaukee south", 
      port =="port washington primary cleaning station" ~ "port washington",
      port =="port washington cleaning station" ~ "port washington",
      port =="port washington north slip" ~ "port washington",
      port =="sheboygan south pier" ~ "sheboygan",
      port =="sheboygan deland park" ~ "sheboygan",
      port =="racine reefpoint marina" ~ "racine",
      port =="racine primary cleaning station" ~ "racine",
      TRUE ~ port)
    ) |> 
  left_join(xref_ports[,3:4], by = "port") |> 
  left_join(xref_compartments, by = "taxon_code") |> 
  # Standardize columns
  mutate(
    dataset = "nps_2015",
    sample_id = as.character(sample_id),
    date = lubridate::mdy(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    depth_m = NA,
    num_ind = NA,
    mass_g = weight_kg * 1000
    ) |>
  select(-weight_kg) |> 
  relocate_columns()



## GLFT 2016 salmonids =========================================================

# Read data
(raw_glft_2016_fish <- 
  read_xlsx(
    here("data","raw_turschak_maitland_query.xlsx"), 
    sheet = "2016_GLFT_Data_Query"
    ) |> 
  cleans_names_and_caps() )

df_glft_2016_fish <- 
  raw_glft_2016_fish |> 
  select(
    sample_id,
    date, 
    port = port_location, 
    depth_m, 
    taxon_specific = species, 
    length_mm = tl,
    d15n, 
    d13c, 
    ug_n = n_ug, 
    ug_c = c_ug
  ) |> 
  mutate(cn = ug_c / ug_n) |>
  select(-ug_c, -ug_n) |> 
  # rename a few ports to match the xref table
  mutate(
    port = case_when(
      port =="strugeon bay" ~ "sturgeon bay", 
      port =="port washington primary cleaning station" ~ "port washington",
      port =="port washington cleaning station" ~ "port washington",
      port =="port washington north slip" ~ "port washington",
      port =="sheboygan south pier" ~ "sheboygan",
      port =="sheboygan deland park" ~ "sheboygan",
      port =="racine reefpoint marina" ~ "racine",
      port =="racine primary cleaning station" ~ "racine",
      TRUE ~ port), 
    taxon_specific = case_when(
      taxon_specific == "nine-spine stickleback" ~ "ninespine stickleback", 
      TRUE ~ taxon_specific
      )
    ) |> 
  left_join(xref_ports[,3:4], by = "port") |> 
  left_join(xref_compartments, by = "taxon_specific") |> 
  mutate(
    dataset = "glft_2016",
    mass_g = NA, 
    num_ind = NA,
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1))
    ) |> 
  mutate(depth_m = as.numeric(depth_m)) |>
  relocate_columns()



## CSMI 2015 ===================================================================

# Read data
(raw_csmi_2015 <- 
  read_xlsx(
    here("data","raw-CSMI-2015.xlsx"), 
    sheet = "Combined UF MED"
  ) |> 
  cleans_names_and_caps() )

df_csmi_2015 <- raw_csmi_2015 |> 
  select(
    sample_id, 
    season,
    site_code = site,
    depth_m,
    taxon_code = species_o, 
    d15n, 
    d13c = d13cb,
    cn = c_n
    ) |> 
  mutate(taxon_code = ifelse(taxon_code=="ltr", "lat", taxon_code)) |> 
  left_join(xref_ports[,2:4] |> filter(!is.na(site_code)), by = "site_code") |> 
  left_join(xref_compartments, by = "taxon_code") |> 
  select(-site_code) |> 
  mutate(
    dataset = "csmi_2015",
    length_mm = NA, 
    mass_g = NA, 
    date = NA,
    num_ind = NA,
    year = 2015
    ) |> 
  relocate_columns()



## Roth 2019 ===================================================================

# We do not have metadata for site locations/ports. Finest grain spatial 
# resolution in lake region

(raw_roth_2019_fish <- read_csv(
  here("data", "raw-Roth-2019.csv")
  ) |> 
  cleans_names_and_caps() )

df_roth_2019_fish <- raw_roth_2019_fish |>
  select(
    sample_id = msu_id, 
    date, 
    site_code = cap_site, 
    depth_m,   
    lake_region = region,
    taxon_code = species,
    d15n = del15n,  
    d13c = del13c, 
    cn = c_n_ratio,
    length_mm = t_lmm, 
    mass_g = t_wg
    )



df_roth_2019_fish <- df_roth_2019_fish |> 
  mutate(
    taxon_code = case_when(
      taxon_code=="dre" ~ "mus", 
      taxon_code=="smt" ~ "ras", 
      taxon_code=="rgb" ~ "rog", 
      TRUE ~ taxon_code), 
    lake_region = case_when(
      lake_region %in% c("nem") ~ "nw", 
      lake_region %in% c("sem") ~ "se", 
      lake_region %in% c("nwm") ~ "nw", 
      lake_region %in% c("swm") ~ "sw", 
      TRUE ~ "Huron"
      )
    ) |> 
  left_join(xref_compartments, by = "taxon_code") |> 
  select(-site_code) |> 
  mutate(
    dataset = "roth_2019",
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)),
    port = NA,
    date = NA,
    num_ind = NA,
    year = case_when(is.na(year) ~ 2019, TRUE ~ 2019)
    ) |> 
  relocate_columns()










