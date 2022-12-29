
# Read in raw datasets and standardize
# Combine datasets
# Clean taxonomy and assiagn factors
# Apply lipid normalization
# Apply body mass conversions


# Notes:
# Each dataset is different. Some preprocessing has been done 
# to the data files. If something was changed manually, it was flagged in 
# a new column on the data. 


## Prep ========================================================================

# Libraries, xrefs, and helpers
source(here::here("R", "00_prep.R"))

## Compile data ===========
### CSMI 2015 ==================================================================

# Notes: 
# - data not lipid corrected
# - added flag column to raw data file to remove flagged data
# - adding / simulating length data

# # Read raw data
# raw_csmi_2015 <-
#   read_xlsx(
#     here("data","raw-CSMI-2015.xlsx"),
#     sheet = "Combined UF MED"
#   ) |>
#   cleans_names_and_caps()
# 
# # Variable selection and filter flagged data
# df_csmi_2015 <-
#   raw_csmi_2015 |>
#   filter(!flag==1) |>
#   select(
#     sample_id,
#     season,
#     port = site,
#     length_mm,
#     depth_m,
#     species_code = species,
#     species_group = species_o,
#     d15n,
#     d13c = d13cb,
#     cn = c_n
#   )
# 
# 
# # Clean location info and add lake regions
# df_csmi_2015 <- 
#   df_csmi_2015 |>
#   mutate(
#     port = case_when(
#       port == "0" ~ NA_character_,
#       port == "arc" ~ "arcadia",
#       port == "lars" ~ "st joseph",
#       port == "lud" ~ "ludington",
#       port == "man" ~ "manitowoc",
#       port == "mid" ~ "midlake",
#       port == "rac" ~ "racine",
#       port == "sau" ~ "saugatuck",
#       port == "stb" ~ "sturgeon bay",
#       port == "stj" ~ "st joseph",
#       port == "wak" ~ "waukegan")
#     ) |>
#   left_join(xref_ports[,3:4], by = "port")
# 
# # read species code definitions
# csmi_2015_spp_xref <-
#   read_xlsx(
#     here("data","raw-CSMI-2015.xlsx"),
#     sheet = "Definitions",
#     skip = 22
#   ) |>
#   select(species_code = 2, species = 3) |>
#   slice_head(n=32) |>
#   slice(-27) |>   # ZOP duplicate
#   cleans_names_and_caps() |>
#   mutate(
#     species = case_when(
#       species_code == "byt" ~ "bythotrephes",
#       species_code == "dws" ~ "deepwater sculpin",
#       species_code == "lwf" ~ "lake whitefish",
#       species_code == "mus" ~ "dreissena",
#       species_code == "zop153" ~ "zooplankton153",
#       species_code == "zop63" ~ "zooplankton63",
#       species_code == "zop" ~ "zooplankton",
#       TRUE ~ species
#     )
#   )
# 
# # add species definitions to data
# df_csmi_2015 <-
#   df_csmi_2015 |>
#   left_join(csmi_2015_spp_xref, by = "species_code") |>
#   mutate(
#     species = case_when(
#       species_code == "pom" ~ "pom",
#       species_code == "dip" ~ "deepwater sculpin ip",
#       species_code == "zop" & season %in% c(1,2) ~ "zooplankton63",
#       species_code == "zop" & season %in% c(3) ~ "zooplankton240",
#       TRUE ~ species
#   )) |>
#   mutate(
#     species_group = case_when(
#       species_group == "alel" ~ "alewife lg",
#       species_group == "ales" ~ "alewife sm",
#       species_group == "blol" ~ "bloater lg",
#       species_group == "blos" ~ "bloater sm",
#       species_group == "ha" ~ "hemimysis adult",
#       species_group == "hj" ~ "hemiysis juv",
#       species_group == "musl" ~ "dreissena lg",
#       species_group == "musm" ~ "dreissena md",
#       species_group == "muss" ~ "dreissena sm",
#       species_group == "smsl" ~ "slimy sculpin lg",
#       species_group == "smss" ~ "slimy sculpin sm",
#       species_group == "zop153" ~ "zooplankton153",
#       species_group == "zop63" ~ "zooplankton63",
#       species == "zooplankton240" ~ "zooplankton240",
#       species == "zooplankton153" ~ "zooplankton153",
#       species == "zooplankton63" ~ "zooplankton63",
#       TRUE ~ species
#   )) |>
#   select(-species_code) |>
#   relocate(species, .before = species_group)
# 
# 
# # assign trophic compartments
# df_csmi_2015 <- 
#   df_csmi_2015 |>
#   mutate(compartment = case_when(
#     species %in% c("pom") ~ "pom",
#     species %in% c("algae") ~ "macro-alga",
#     species %in% c(
#       "zooplankton240", "zooplankton153", "zooplankton63", "bythotrephes",
#       "hemimysis","mysis") ~ "zooplankton",
#     species %in% c(
#       "amphipod","chironomids","oligochaete","crayfish") ~ "benthic inverts",
#     species %in% c("dreissena") ~ "dreissenids",
#     species %in% c(
#       "bloater ip","alewife ip","deepwater sculpin ip") ~ "ichthoplankton",
#     TRUE ~ "fishes"
#   ))
# 
# 
# 
# #### CSMI body size data -----------------------------------------------------
# 
# # I manually imputed lengths for individual fish I could ID between datasets
# # That was mostly for lake trout, lake whitefish, burbot
# # So this procedure is mostly for alewife, bloater, slimys, dwcs,
# 
# # Load length data
# source(here("R", "load_csmi_legnths.R"))
# 
# # Join distribution summary stats to csmi data
# df_csmi_2015 <- df_csmi_2015 |>
#   left_join(
#     csmi_lengths_dists, by = c(
#       "species_group"="species_name",
#       "season",
#       "port"="port_name",
#       "depth_m"="station_depth")
#   )
# 
# # Simulate lengths - using distribution stats
# df_csmi_2015_lengthed <- 
#   df_csmi_2015 |>
#   mutate(seed = 1:nrow(df_csmi_2015)+10) %>%
#   mutate(
#     length_mm = case_when(
#       compartment=="fishes" & is.na(length_mm) ~
#         pmap_dbl(list(mean, sd, seed), function(x, y, z){
#           set.seed(z); rnorm(1, x, y)
#           }
#           ),
#       TRUE ~ length_mm
#       )
#     ) |>
#   select(-mean, -sd, -seed)
# 
# # Fix NaN
# df_csmi_2015_lengthed <-
#   df_csmi_2015_lengthed |>
#   mutate_all(~ifelse(is.nan(.), NA, .))
# 
# # From 15% to ~70% now have lengths
# 
# saveRDS(df_csmi_2015_lengthed, here("out","data","df_csmi_2015_lengthed.rds"))

# Load csmi data that has length data attributed to fishes
df_csmi_2015_lengthed <- readRDS(here("out","data","df_csmi_2015_lengthed.rds"))

### Standardize data

df_csmi_2015_clean <-
  df_csmi_2015_lengthed |>
  mutate(
    dataset = "csmi_2015",
    mass_g = NA,
    date = NA,
    num_ind = NA,
    year = 2015
  ) |>
  relocate_columns()


### UWM 2002 fish ==============================================================


# Read data
raw_uwm_2002_fish <- 
  read_xlsx(
    here("data","raw_turschak_maitland_query.xlsx"), 
    sheet = "2002-2003 SIA Data") |> 
  cleans_names_and_caps()

# Variable selection and flags
df_uwm_2002_fish <- raw_uwm_2002_fish |> 
  filter(!flag==1) |> 
  select(
    sample_id,
    date, 
    port = port_location, 
    depth_m, 
    species, 
    length_mm = tl,
    d15n, 
    d13c, 
    ug_n = n_ug, 
    ug_c = c_ug
  ) |> 
  mutate(cn = ug_c / ug_n) |> 
  select(-ug_c, -ug_n) 

# Location info
df_uwm_2002_fish <- df_uwm_2002_fish |> 
  mutate(
    port = case_when(
      port =="s1" ~ "whitefish bay", 
      port =="s2" ~ "dead river",
      port =="s3" ~ "michigan city", 
      port =="s4" ~ "muskegon",
      TRUE ~ port
      )
    ) |> 
  # get lake regions for port xref
  left_join(xref_ports[,3:4], by = "port") 

# Species and compartments
df_uwm_2002_fish <- df_uwm_2002_fish |> 
  mutate(species = ifelse(species %in% c("quagga", "zebra"), "dreissena", species)) |> 
  mutate(species_group = species) |> 
  mutate(compartment = case_when(
    species == "dreissena" ~ "dreissenids", 
    species == "zooplankton" ~ "zooplankton", 
    TRUE ~ "fishes"))


# Standardize columns
df_uwm_2002_fish <- df_uwm_2002_fish |> 
  mutate(
    dataset = "uwm_2002_2003",
    mass_g = NA, 
    num_ind = NA,
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(
      date, with_year = FALSE, fiscal_start = 1))
    ) |>
  relocate_columns()


### UWM 2010 fish =============================================================


# Read data
raw_uwm_2010_fish <-
  read_xlsx(
    here("data","raw_bootsma_2010_2011_fish.xlsx"),
    sheet = "FishSIResults2010_2011")|>
  cleans_names_and_caps()

#  Variable selection
df_uwm_2010_fish <- raw_uwm_2010_fish |> 
  select(
    sample_id = sample_num,
    date, 
    site_code = site_name, 
    depth_m = site_depth_general, 
    species = fish_species, 
    length_mm = fish_tl,
    d15n = corrected_n_d_29_28, 
    d13c = corrected_c_d_13_12, 
    ug_n, ug_c
  ) |> 
  mutate(cn = ug_c / ug_n) |> 
  select(-ug_c, -ug_n)

# Locations / ports
df_uwm_2010_fish <- df_uwm_2010_fish |> 
  mutate(
    port = case_when(
      site_code == "fpt" ~ "fox point",  
      site_code == "we-reef" ~ "sheboygan reef",  
      site_code == "gc" ~ "green can",  
      site_code == "1r" ~ "fox point",  
      site_code == "1s" ~ "whitefish bay",  
      site_code == "2r" ~ "highland park",  
      site_code == "2s" ~ "dead river",  
      site_code == "3r" ~ "calumet",  
      site_code == "3s" ~ "michigan city",  
      site_code == "4r" ~ "saugatuck",  
      site_code == "4s" ~ "muskegon",  
      site_code == "5r" ~ "arcadia",  
      site_code == "6r" ~ "sturgeon bay", 
      TRUE ~ NA_character_
      )
    ) |> 
  select(-site_code) |> 
  left_join(xref_ports[,3:4], by = "port")

# Species 
df_uwm_2010_fish <- df_uwm_2010_fish |> 
  mutate(
    species = case_when(
      species == "9 spine stickleback" ~ "ninespine stickleback", 
      species == "3 spine stickleback" ~ "threespine stickleback", 
      species == "shorthead river redhorse" ~ "shorthead redhorse", 
      TRUE ~ species
      ))
  
# Standardize columns
df_uwm_2010_fish <- df_uwm_2010_fish |> 
  mutate(
    dataset = "uwm_2010_2011",
    compartment = "fishes", 
    species_group = species,
    num_ind = NA,
    length_mm = ifelse(length_mm == 99999, NA, length_mm),
    mass_g = NA, 
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    # convert the text depth labels to numeric
    depth_m = case_when(
      depth_m == "shallow" ~ 3, 
      depth_m == "intermediate" ~ 8, 
      depth_m == "deep" ~ 15, 
      depth_m == "various" ~ as.double(NA)
    )
    ) |>
  mutate(year = ifelse(is.na(date), 2010, year)) |> 
  relocate_columns()

# skim(df_uwm_2010_fish)

### UWM 2010 benthic =========================================================

# Notes: 
# - I filled in missing site data for much easier data cleaning
# - all zoops are 63 um mesh

# Read data
raw_uwm_2010_benthic <- read_xlsx(
  here(
    "data",
    "raw_Nearshore seston and benthos stable isotopes 2010 for Bryan Maitland.xlsx"), 
  sheet = "Benthic", 
  skip = 6
  ) |>
  cleans_names_and_caps() 


# Variable selection and flags
df_uwm_2010_benthic <- raw_uwm_2010_benthic |> 
  filter(!flag==1) |> 
  select(
    sample_id = sample_number, 
    date = date_collected,
    site, 
    depth_m, 
    species = taxa, 
    num_ind = number_of_individuals, 
    mass_g = dry_mass_g, 
    d15n = d_29_28, 
    d13c = d_13_12, 
    ug_c, ug_n
  ) |> 
  mutate(cn = ug_c / ug_n) |>
  select(-ug_c, -ug_n)


# Deal with cluster f*ck of site/port names and standardize
df_uwm_2010_benthic <- df_uwm_2010_benthic |> 
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
    into = c("a", "b", "site_code", "d"), 
    sep = c(2,4, 6),
    extra = "merge", 
    remove = FALSE
    ) |> 
  mutate(
    site_code = case_when(
      a == "4r" ~ "4r", 
      is.na(site_code) & port == "sturgeon bay" ~ "6r", 
      is.na(site_code) & port == "fox point" ~ "1s", 
      is.na(site_code) & port == "whitefish bay" ~ "1s", 
      TRUE ~ site_code)
    ) |> 
  select(-a, -b, -d, -port) |> 
  mutate(
    port = case_when(
      site_code == "fpt" ~ "fox point",  
      site_code == "we-reef" ~ "sheboygan reef",  
      site_code == "gc" ~ "green can",  
      site_code == "1r" ~ "fox point",  
      site_code == "1s" ~ "whitefish bay",  
      site_code == "2r" ~ "highland park",  
      site_code == "2s" ~ "dead river",  
      site_code == "3r" ~ "calumet",  
      site_code == "3s" ~ "michigan city",  
      site_code == "4r" ~ "saugatuck",  
      site_code == "4s" ~ "muskegon",  
      site_code == "5r" ~ "arcadia",  
      site_code == "6r" ~ "sturgeon bay", 
      TRUE ~ NA_character_
    )
  ) |> 
  left_join(xref_ports[,c(3:4)], by = "port") |> 
  select(-site_code)


# Species 
# df_uwm_2010_benthic |> count(species)

df_uwm_2010_benthic <- df_uwm_2010_benthic |> 
  mutate(species = case_when(
    str_detect(species, "algae") ~ "algae", 
    str_detect(species, "amphipod") ~ "amphipod", 
    str_detect(species, "chironomid") ~ "chironomids", 
    str_detect(species, "crayfish") ~ "crayfish", 
    str_detect(species, "cladophora") ~ "algae", 
    str_detect(species, "dipteran") ~ "chironomids", 
    str_detect(species, "hydracarina") ~ "hydracarina", 
    str_detect(species, "isopod") ~ "isopod", 
    str_detect(species, "leech") ~ "leech", 
    str_detect(species, "oligochaete") ~ "oligochaete", 
    str_detect(species, "quagga") ~ "dreissena", 
    str_detect(species, "zooplankton") ~ "zooplankton63"
  )) |> 
  mutate(
    species_group = species, 
    compartment = case_when(
      species %in% c("zooplankton63") ~ "zooplankton", 
      species %in% c("algae") ~ "macro-alga", 
      species %in% c("dreissena") ~ "dreissenids", 
      is.na(species) ~ NA_character_, 
      TRUE ~ "benthic inverts"
  ))


# Standardize 
df_uwm_2010_benthic <- df_uwm_2010_benthic |> 
  mutate(
    dataset = "uwm_2010_2011",
    date = lubridate::as_date(date),
    length_mm = NA_integer_, 
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)), 
    depth_m = as.numeric(ifelse(depth_m == "integrated", "8", depth_m)),
    mass_g = as.numeric(mass_g),
    num_ind = as.numeric(ifelse(num_ind == "n/a", NA_character_, num_ind))) |>
  relocate_columns()


### UWM 2010 seston =====================================


# # Read data
# raw_uwm_2010_benthic <- read_xlsx(
#   here(
#     "data",
#     "raw_Nearshore seston and benthos stable isotopes 2010 for Bryan Maitland.xlsx"),
#   sheet = "Water",
#   skip = 6
#   ) |>
#   cleans_names_and_caps()
# 
# 
# # Variable selection and flags
# df_uwm_2010_benthic <- raw_uwm_2010_benthic |>
#   filter(!flag==1) |>
#   select(
#     sample_id = sample_number,
#     date = date_collected,
#     site,
#     depth_m,
#     species = "pom",
#     num_ind = number_of_individuals,
#     mass_g = dry_mass_g,
#     d15n = d_29_28,
#     d13c = d_13_12,
#     ug_c, ug_n
#   )




### Kornis 2014  ==========================================================

# Read data
raw_kornis_2014 <- 
  read_xlsx(
    here("data","raw_Kornis et al 2014 isotope data.xlsx"), 
    sheet = "Kornis et al. data spreadsheet"
  ) |> 
  cleans_names_and_caps()


# Variable selection
df_kornis_2014 <-
  raw_kornis_2014 |> 
  select(
    sample_id = specimen_id,
    date = sample_date, 
    season = season,
    lake_region = region,
    port = landing_site, 
    species, 
    length_mm = total_length_mm,
    mass_g = weight_g,
    d15n = raw_d15n, 
    d13c = raw_d13c, 
    cn = c_n_ratio
  ) 

# Fix regions / port names
df_kornis_2014 <- 
  df_kornis_2014 |> 
  mutate(
    lake_region = case_when(
      lake_region =="northeast" ~ "ne", 
      lake_region =="northwest" ~ "nw",
      lake_region =="southeast" ~ "se",
      lake_region =="southwest" ~ "sw",
      port == "ludington" ~ "ne", 
      port == "manistique" ~ "nw", 
      port == "port washington" ~ "sw", 
      TRUE ~ lake_region
      ), 
    port = ifelse(port == "st. joseph", "st joseph", port)
  )

# Fix species names 
df_kornis_2014 <- 
  df_kornis_2014 |> 
  mutate(
    species = case_when(
      species == "large alewife" ~ "alewife", 
      species == "rainbow smelt" ~ "rainbow smelt", 
      species == "nearshore bloater" ~ "bloater", 
      species == "nearshore round goby" ~ "round goby", 
      species == "offshore bloater" ~ "bloater",
      species == "small rainbow smelt" ~ "rainbow smelt",
      species == "large rainbow smelt" ~ "rainbow smelt",
      species == "steelhead" ~ "rainbow trout",
      species == "quagga mussel" ~ "dreissena", 
      TRUE ~ species
      )
    ) 

# Compartments
df_kornis_2014 <- df_kornis_2014 |> 
  mutate(
    compartment = case_when(
      species == "dreissenia" ~ "dreissenids", 
      species == "mysis" ~ "zooplankton", 
      TRUE ~ "fishes"
      )
  ) 

# Standardize
df_kornis_2014 <- df_kornis_2014 |> 
    mutate(
      dataset = "kornis_2014",
      sample_id = as.character(sample_id),
      date = lubridate::as_date(date),
      year = lubridate::year(date), 
      season = as.character(lubridate::quarter(
        date, with_year = FALSE, fiscal_start = 1)), 
      depth_m = NA,
      species_group = species, 
      num_ind = NA,
    ) |>
    relocate_columns()

# only keep salmonines for now 
df_kornis_2014 <- df_kornis_2014 |> 
  filter(species %in% c(
    "brown trout", 
    "chinook salmon", 
    "coho salmon",
    "rainbow trout", 
    "lake trout"
    ))
  


### NPS 2015 salmonids ========================================================

# Read data
raw_nps_2015_salmonids <- 
  read_csv(
    here("data", "raw_Salmonine_Isotope_2015.csv")
    ) |> 
  cleans_names_and_caps()

# Variable selection
df_nps_2015_salmonids <-
  raw_nps_2015_salmonids |> 
  select(
    sample_id = id,
    date = collection_date, 
    port = port_where_fish_was_landed, 
    species = species, 
    length_mm = tl_mm,
    weight_kg = wt_kg,
    d15n = d15n_14n, 
    d13c = d13c_12c, 
    cn
  ) 

# Link xref tables
df_nps_2015_salmonids <- df_nps_2015_salmonids |> 
  # rename ports to match the xref table
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
  left_join(xref_ports[,3:4], by = "port") 


df_nps_2015_salmonids <- df_nps_2015_salmonids |> 
  mutate(species = case_when(
    species == "rbt" ~ "rainbow trout", 
    species == "bnt" ~ "brown trout", 
    species == "cos" ~ "coho salmon", 
    species == "chs" ~ "chinook salmon", 
    species == "lat" ~ "lake trout", 
  )) 

# Standardize
df_nps_2015_salmonids <- df_nps_2015_salmonids |> 
  mutate(
    dataset = "nps_2015",
    sample_id = as.character(sample_id),
    date = lubridate::mdy(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(
      date, with_year = FALSE, fiscal_start = 1)), 
    depth_m = NA,
    compartment = "fishes", 
    species_group = species, 
    num_ind = NA,
    mass_g = weight_kg * 1000
    ) |>
  select(-weight_kg) |> 
  relocate_columns()



### GLFT 2016 salmonids =======================================================


# Read data
raw_glft_2016_fish <- 
  read_xlsx(
    here("data","raw_turschak_maitland_query.xlsx"), 
    sheet = "2016_GLFT_Data_Query"
    ) |> 
  cleans_names_and_caps()

# Variable selection
df_glft_2016_fish <- raw_glft_2016_fish |> 
  select(
    sample_id,
    date, 
    port = port_location, 
    depth_m, 
    species, 
    length_mm = tl,
    d15n, 
    d13c, 
    ug_n = n_ug, 
    ug_c = c_ug
  ) |> 
  mutate(cn = ug_c / ug_n) |>
  select(-ug_c, -ug_n) 

# Link to xrefs
df_glft_2016_fish <- df_glft_2016_fish |> 
  # rename ports to match the xref table
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
    ) |> 
  left_join(xref_ports[,3:4], by = "port") 

# Standardize
df_glft_2016_fish <- df_glft_2016_fish |> 
  mutate(
    dataset = "glft_2016",
    mass_g = NA, 
    num_ind = NA,
    date = lubridate::as_date(date),
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(
      date, with_year = FALSE, fiscal_start = 1)), 
    compartment = "fishes", 
    species = ifelse(
      species == "nine-spine stickleback", "ninespine stickleback", species), 
    species_group = species
    ) |> 
  mutate(depth_m = as.numeric(depth_m)) |>
  relocate_columns()



### Roth 2019 ==================================================================


# Read data
raw_roth_2019_fish <- read_csv(
  here("data", "raw-Roth-2019.csv")
  ) |> 
  cleans_names_and_caps() 

# SIte metadata
xref_roth_ports <- 
  read_xlsx(here("data","roth-2019-site-codes.xlsx")) |> 
  cleans_names_and_caps()


# Variable selection
df_roth_2019_fish <- raw_roth_2019_fish |>
  select(
    sample_id = msu_id, 
    date, 
    site_code = cap_site, 
    depth_m,   
    lake_region = region,
    species,
    d15n = del15n,  
    d13c = del13c, 
    cn = c_n_ratio,
    length_mm = t_lmm, 
    mass_g = t_wg
    )

# Link to xrefs
df_roth_2019_fish <- df_roth_2019_fish |> 
  mutate(
    lake_region = case_when(
      lake_region %in% c("nem") ~ "nw", 
      lake_region %in% c("sem") ~ "se", 
      lake_region %in% c("nwm") ~ "nw", 
      lake_region %in% c("swm") ~ "sw", 
      TRUE ~ "Huron"
      )) |> 
  left_join(xref_roth_ports |> select(code, port), by = c("site_code" = "code")) 

df_roth_2019_fish <- df_roth_2019_fish |> 
  mutate(species = case_when(
    species == "ale" ~ "alewife", 
    species == "ats" ~ "atlantic salmon", 
    species == "blo" ~ "bloater", 
    species == "bnt" ~ "brown trout", 
    species == "chs" ~ "chinook salmon", 
    species == "cos" ~ "coho salmon", 
    species == "dre" ~ "dreissena", 
    species == "dws" ~ "deepwater sculpin", 
    species == "gzs" ~ "gizzard shad", 
    species == "inv" ~ "oligo-chiro", 
    species == "lat" ~ "lake trout", 
    species == "nsb" ~ "ninespine stickleback", 
    species == "pks" ~ "pks", 
    species == "rbt" ~ "rainbow trout", 
    species == "rgb" ~ "round goby", 
    species == "sls" ~ "slimy sculpin",
    species == "smt" ~ "rainbow smelt",
    species == "sts" ~ "spottail shiner",
    species == "wae" ~ "walleye",
    species == "yep" ~ "yellow perch",
  ))

# Standardize 
df_roth_2019_fish <- df_roth_2019_fish |> 
  select(-site_code) |>
  mutate(
    dataset = "roth_2019",
    year = lubridate::year(date),
    season = as.character(
      lubridate::quarter(date, with_year = FALSE, fiscal_start = 1)),
    # port = NA,
    compartment = case_when(
      species %in% c("oligo-chiro", "dreissena") ~ "benthic inverts", 
      species %in% c("dreissena") ~ "dressenids",
      TRUE ~ "fishes"),
    num_ind = NA,
    species_group = species, 
    year = case_when(is.na(year) ~ 2019, TRUE ~ 2019)
    ) |> 
  relocate_columns()


# Remove Lake Huron data and inverts
df_roth_2019_fish <- df_roth_2019_fish |> 
  filter(lake_region != "Huron") |> 
  filter(species != "oligo-chiro")  |> 
  filter(species != "dreissena")




## Combine data =========================================================

data <- bind_rows(
  df_uwm_2002_fish, 
  df_uwm_2010_fish, 
  df_uwm_2010_benthic, 
  df_kornis_2014,
  df_csmi_2015_clean,
  df_nps_2015_salmonids,
  df_glft_2016_fish,
  df_roth_2019_fish,
) 


## Clean taxonomy =========================================================

data <- data |> 
  mutate(
    species = case_when(
      species == "algae" ~ "cladophora",
      species == "bloater ip" ~ "bloater",
      species == "alewife ip" ~ "alewife",
      species == "deepwater sculpin ip" ~ "deepwater sculpin", 
      TRUE ~ species
      ), 
    species_group = case_when(
      compartment == "macro-alga" ~"cladophora", 
      species == "dreissena" ~ "dreissena", 
      species_group == "alewife" & length_mm < 100 ~ "alewife sm", 
      species_group == "alewife" & length_mm >= 100 ~ "alewife lg", 
      species_group == "bloater" & length_mm < 120 ~ "bloater sm", 
      species_group == "bloater" & length_mm >= 120 ~ "bloater lg", 
      species_group == "slimy sculpin" & length_mm < 40 ~ "slimy sculpin sm", 
      species_group == "slimy sculpin" & length_mm >= 40 ~ "slimy sculpin lg", 
      species_group == "burbot" & length_mm < 250 ~ "burbot sm", 
      species_group == "burbot" & length_mm >= 250 ~ "burbot lg", 
      species_group == "round goby" & length_mm < 75 ~ "round goby sm", 
      species_group == "round goby" & length_mm >= 75 ~ "round goby lg", 
      species_group == "yellow perch" & length_mm < 100 ~ "yellow perch sm", 
      species_group == "yellow perch" & length_mm >= 100 ~ "yellow perch lg", 
      TRUE ~ species_group
    )
    )

# add scientific names
data <- data |> 
  left_join(xref_sci_names, by = c("species"="common_name")) |> 
  relocate(sci_name, .before = species)


# Assign factors =========================================================

data <- data |> 
  select(-date) |> 
  mutate(
    depth_g = case_when(
      depth_m <= 30 ~ "photic", 
      depth_m > 30 & depth_m <= 90 ~ "mid-depth", 
      depth_m > 90 ~ "profundal", 
      TRUE ~ "offshore"
    )
  ) |> 
  
  mutate(
    dataset = factor(
      dataset, 
      levels = c(
        "uwm_2002_2003","uwm_2010_2011","kornis_2014",
        "csmi_2015","nps_2015","glft_2016","roth_2019"
      )), 
    compartment = factor(
      compartment, levels = c(
        "pom", "macro-alga","benthic inverts",
        "dreissenids","ichthoplankton","zooplankton","fishes"
      )), 
    lake_region = factor(
      lake_region, 
      levels = c("nw", "ne", "sw", "se")), 
    season = factor(season, levels = c(1,2,3,4)), 
    depth_g = factor(depth_g, 
                     levels = c("photic","mid-depth","profundal"))
  ) |> 
  mutate(across(where(is.character), as.factor)) |> 
  mutate(across(c(season), as.numeric)) 




## Lipid normalization ========================================================

data <- data |>
  mutate(
    d13c_norm = case_when(
      # non-zoop inverts = Logan et al. (2008)
      compartment == "benthic inverts" ~ 
        0.967 * d13c + 0.861,
      # zoops = Smyntek et al. (2007)
      compartment == "zooplankton" ~ 
        d13c + (6.3 * ((cn - 4.2)/(cn))),
      # fishes = Hoffman et al. (2015)
      compartment %in% c("fishes", "ichthoplankton") & cn > 4 ~
        d13c + (-6.5 * (3.5 - cn)) / cn,
      TRUE ~ d13c
    )
  )


## Outliers  ==================================================================

# data |>
#   filter(d13c < -10) |> 
#   filter(d13c > -40) |>  
#   ggplot(aes(d13c)) + geom_boxplot()
# data |> ggplot(aes(d15n)) + geom_boxplot()
# data |> ggplot(aes(cn)) + geom_boxplot()
# data |> ggplot(aes(length_mm)) + geom_boxplot()
# data |> ggplot(aes(mass_g)) + geom_boxplot()

data <- data |> 
  filter(d13c < -10) |> 
  filter(d13c > -40) |> 
  filter(cn < 30)



# Convert body size   ========================================================

# load log a and b parameters from L-W regressions (02_Bayes_LWR_Model.R)
df_lw_params <- read_rds(here("out", "tbls", "body_mass_params.rds"))

# Join params to data
data <- data |> 
  left_join(df_lw_params, by = "sci_name") |> 
  mutate(across(c(sci_name), as.factor)) 

# Convert
data <- 
  data |> 
  mutate(length_cm = length_mm / 10) |>  # need cm for conversion
  mutate(mass_g = case_when(
    is.na(mass_g) & (!is.na(log_a)) ~ 10^(log_a + b * (log10(length_cm))), 
    TRUE ~ mass_g
  )) |> 
  select(-log_a, -b, -length_cm)


# Add body mass average values for other groups / species

data <- data |> 
  mutate(mass_g = case_when(
    species == "amphipod" ~ 0.001, 
    species == "chironomids" ~ 0.0002, 
    species == "crayfish" ~ 5, 
    species == "hydracarina" ~ 0.002, 
    species == "isopod" ~ 0.01, 
    species == "leech" ~ 1, 
    species == "oligochaete" ~ .1, 
    species == "dreissena" ~ .1,
    species == "bythotrephes" ~ 2,
    species == "hemimysis" ~ 0.01,
    species == "mysis" ~ 3,
    species == "zooplankton" ~ .5,
    species == "zooplankton240" ~ .2,
    species == "zooplankton153" ~ .1,
    species == "zooplankton63" ~ .01,
    TRUE ~ mass_g
  )) 


# Missing data from:
# data |>
#   filter(is.na(mass_g)) |> 
#   count(compartment, sci_name, species, species_group) |>
#   arrange(compartment, species) |>
#   print(n=100)

# Save compiled data =============================================

save(data, file = here("out", "data", "compiled_data.RData"))
# load(here("out", "data", "compiled_data.RData"))

