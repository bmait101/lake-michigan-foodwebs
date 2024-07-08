
# Read in raw datasets and standardize
# Apply lipid normalization


# Notes:
# Each dataset is different. Some preprocessing has been done 
# to the data files. If something was changed manually, it was flagged in 
# a new column on the data. 


## Prep ========================================================================

# Libraries, xrefs, and helpers
source(here::here("R", "00_prep.R"))

## Import & clean =============================================================
### CSMI 2015 ==================================================================

# Notes: 
# - data not lipid corrected
# - adding / simulating length data for missing body sizes

# read raw data (n=1792)
raw_csmi_2015 <-
  read_xlsx(
    here("data-raw","raw-CSMI-2015.xlsx"),
    sheet = "Combined UF MED"
  ) |>
  cleans_names_and_caps()

# skim it
# skim(raw_csmi_2015)
# missing SI for 20 samples, plus missing species in for for 8

# Removed rows with missing SI data
df_csmi_2015 <- raw_csmi_2015 |> 
  filter(if_all(c(d15n, d13cb), ~ !is.na(.))) 

# Check for rows to remove in notes
# df_csmi_2015 |> 
#   filter(!is.na(op_notes)) |> 
#   select(species, op_notes) |> 
#   print(n=Inf)

# remove rows with potentially bad data (n=6) (but keep NAs)
df_csmi_2015 <- df_csmi_2015 |> 
  filter(
    is.na(op_notes) | 
      str_detect(op_notes, "insuf|n below detection", negate = TRUE))

# skim again
# skim(df_csmi_2015)
# still missing taxon for 1 obs.

# remove rows missing taxon (n=1)
df_csmi_2015 <- filter(df_csmi_2015, !is.na(species))

# duplicates
df_csmi_2015 |> get_dupes(sample_id)  # do not appear to be true duplicates, ID must be bulked samples

# Standardize and select fields
df_csmi_2015 <- df_csmi_2015 |>
  select(
    sample_id,
    season,
    port = site,
    length_mm,
    depth_m,
    species_code = species,
    species_group = species_o,
    d15n,
    d13c = d13cb,
    cn = c_n
  )

# check it
# skim(df_csmi_2015)

# why 5 seasons?
# count(df_csmi_2015, season)  # 4 rows with no season data
df_csmi_2015 <- df_csmi_2015 |> filter(!season %in% c(0,"nr"))
# convert season 1 to 2, and 2 to 3 (fiscal quarters)
df_csmi_2015 <- df_csmi_2015 |> 
  mutate(season = case_when(
    season == 1 ~ "spring",
    season == 2 ~ "summer",
    season == 3 ~ "fall"
  ))


# Check spatial data (ports)
# count(df_csmi_2015, port)  # 7 rows with no spatial reference
df_csmi_2015 <- df_csmi_2015 |> 
  filter(!port %in% c("0","z.unk","midlake"))

# clean location info and add lake regions for xref table
df_csmi_2015 <- df_csmi_2015 |>
  mutate(
    port = case_when(
      port == "arc" ~ "arcadia",
      port == "lars" ~ "st joseph", # St. Joes Deep station
      port == "lud" ~ "ludington",
      port == "man" ~ "manitowoc",
      # port == "mid" ~ "midlake",
      port == "rac" ~ "racine",
      port == "sau" ~ "saugatuck",
      port == "stb" ~ "sturgeon bay",
      port == "stj" ~ "st joseph",
      port == "wak" ~ "waukegan")
    ) |>
  left_join(xref_ports[,3:4], by = "port")

# check it
# skim(df_csmi_2015)

# read in species code definitions and clean up
csmi_2015_spp_xref <-
  read_xlsx(
    here("data-raw","raw-CSMI-2015.xlsx"),
    sheet = "Definitions",
    skip = 22
  ) |>
  select(species_code = 2, species = 3) |>
  slice_head(n=32) |>
  slice(-27) |>   # ZOP duplicate
  cleans_names_and_caps() |>
  mutate(
    species = case_when(
      species == "algae" ~ "benthic algae",
      species_code == "byt" ~ "bythotrephes",
      species_code == "dws" ~ "deepwater sculpin",
      species_code == "lwf" ~ "lake whitefish",
      species_code == "mus" ~ "dreissena",
      species_code == "zop153" ~ "zooplankton153",
      species_code == "zop63" ~ "zooplankton63",
      species_code == "zop" ~ "zooplankton",
      TRUE ~ species
    )
  )

# add species definitions to data
df_csmi_2015 <- left_join(df_csmi_2015, csmi_2015_spp_xref, by = "species_code") 

# check species assignments
# df_csmi_2015 |> count(species) |> print(n=Inf)
# missing species labels for 157 samples
# check them
# df_csmi_2015 |> filter(is.na(species)) |> count(species_code)

# add species labels for those missing it
df_csmi_2015 <- df_csmi_2015 |> 
  mutate(
    species = case_when(
      species_code == "dip" ~ "deepwater sculpin ip", 
      species_code == "pom" ~ "pom", 
      TRUE ~ species
    )
  )

# check fix
# df_csmi_2015 |> count(species) |> print(n=Inf)
# df_csmi_2015 |> count(species_code) |> print(n=Inf)

# sping and summer zops are 63 and fall is 240 mesh
df_csmi_2015 <- df_csmi_2015 |>
  mutate(
    species = case_when(
      species_code == "zop" & season %in% c("spring","summer") ~ "zooplankton63",
      species_code == "zop" & season %in% c("fall") ~ "zooplankton240",
      TRUE ~ species
  ))

# assaign and rename species groups based on species codes
df_csmi_2015 <- df_csmi_2015 |>
  mutate(
    species_group = case_when(
      species_group == "alel" ~ "alewife lg",
      species_group == "ales" ~ "alewife sm",
      species_group == "blol" ~ "bloater lg",
      species_group == "blos" ~ "bloater sm",
      species_group == "ha" ~ "hemimysis adult",
      species_group == "hj" ~ "hemiysis juv",
      species_group == "musl" ~ "dreissena lg",
      species_group == "musm" ~ "dreissena md",
      species_group == "muss" ~ "dreissena sm",
      species_group == "smsl" ~ "slimy sculpin lg",
      species_group == "smss" ~ "slimy sculpin sm",
      species == "zooplankton240" ~ "zooplankton240",
      species == "zooplankton153" ~ "zooplankton153",
      species == "zooplankton63" ~ "zooplankton63",
      TRUE ~ species
  )) |>
  select(-species_code) |>
  relocate(species, .before = species_group)

# check taxonomony again
# df_csmi_2015 |> count(species, species_group) |> print(n=Inf)


# assign trophic compartments
df_csmi_2015 <- df_csmi_2015 |>
  mutate(
    compartment = case_when(
      species %in% c("pom") ~ "pom",
      species %in% c("benthic algae") ~ "benthic algae",
      species %in% c(
        "zooplankton240", "zooplankton153", "zooplankton63", "bythotrephes",
        "hemimysis", "mysis") ~ "zooplankton",
      species %in% c(
        "amphipod","chironomids","oligochaete","crayfish") ~ "benthic inverts",
      species %in% c("dreissena") ~ "dreissenids",
      species %in% c(
        "bloater ip","alewife ip","deepwater sculpin ip") ~ "ichthoplankton",
      TRUE ~ "fishes"
  ))


# adding depth - assumed from data and sample ids
df_csmi_2015 <- df_csmi_2015 |> 
  mutate(depth_m = if_else(sample_id == "cmu-4-ale-3-14", 5, depth_m)) |> 
  mutate(depth_m = if_else(sample_id == "63-562", 18, depth_m)) 

# df_csmi_2015 |> count(species) |> print(n=Inf)
# df_csmi_2015 |> count(compartment) |> print(n=Inf)


#### CSMI body size data -----------------------------------------------------

# I manually imputed lengths for individual fish I could ID between datasets
# That was mostly for lake trout, lake whitefish, burbot
# So this procedure is mostly for alewife, bloater, slimys, dwcs,

# df_csmi_2015 |> filter(compartment=="fishes") |> count()
# df_csmi_2015 |> filter(compartment=="fishes", is.na(length_mm)) |> count()
# 
# # Load length data
# source(here("R", "load_csmi_legnths.R"))
# 
# #### Est by species, port, season, and depth 
# df_csmi_2015_lengthed <- df_csmi_2015 |> 
#   left_join(
#     csmi_lengths_season_port_depth, by = c(
#       "species_group"="species_name",
#       "season",
#       "port"="port_name",
#       "depth_m"="station_depth")
#   ) |>
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
#   #fix two negatives
#   mutate(length_mm = ifelse(length_mm<0, length_mm*-1, length_mm)) |> 
#   select(-mean, -sd, -seed) |>
#   mutate_all(~ifelse(is.nan(.), NA, .))  # fix NaN
# 
# df_csmi_2015_lengthed |> filter(compartment=="fishes", is.na(length_mm)) |> count() # 247
# # df_csmi_2015_lengthed |> relocate(c(length_mm, mean, sd), .after = length_mm) |>
# #   filter(compartment=="fishes") |> View()
# 
# #### Est by species, port, season 
# df_csmi_2015_lengthed_2 <- df_csmi_2015_lengthed |> 
#   left_join(
#     csmi_lengths_season_port, by = c(
#       "species_group"="species_name",
#       "season",
#       "port"="port_name")
#   ) |>
#   mutate(seed = 1:nrow(df_csmi_2015_lengthed)+10) %>%
#   mutate(
#     length_mm = case_when(
#       compartment=="fishes" & is.na(length_mm) ~
#         pmap_dbl(list(mean, sd, seed), function(x, y, z){
#           set.seed(z); rnorm(1, x, y)
#         }
#         ),
#       TRUE ~ length_mm
#     )
#   ) |>
#   select(-mean, -sd, -seed) |>
#   mutate_all(~ifelse(is.nan(.), NA, .))  # fix NaN
# 
# df_csmi_2015_lengthed_2 |> filter(compartment=="fishes", is.na(length_mm))|> count() # 208
# # df_csmi_2015_lengthed_2 |> relocate(c(length_mm, mean, sd), .after = length_mm) |>
# #   filter(compartment=="fishes") |> View()
# 
# #### Est by species, port 
# df_csmi_2015_lengthed_3 <- df_csmi_2015_lengthed_2 |> 
#   left_join(
#     csmi_lengths_port, by = c(
#       "species_group"="species_name",
#       "port"="port_name")
#   ) |> 
#   mutate(seed = 1:nrow(df_csmi_2015_lengthed_2)+10) %>%
#   mutate(
#     length_mm = case_when(
#       compartment=="fishes" & is.na(length_mm) ~
#         pmap_dbl(list(mean, sd, seed), function(x, y, z){
#           set.seed(z); rnorm(1, x, y)
#         }
#         ),
#       TRUE ~ length_mm
#     )
#   ) |>
#   select(-mean, -sd, -seed) |>
#   mutate_all(~ifelse(is.nan(.), NA, .))  # fix NaN
# 
# df_csmi_2015_lengthed_3 |> filter(compartment=="fishes", is.na(length_mm))|> count() # 198
# # df_csmi_2015_lengthed_3 |> relocate(c(length_mm, mean, sd), .after = length_mm) |>
# #   filter(compartment=="fishes") |> View()
# 
# 
# #### Est by species, port 
# df_csmi_2015_lengthed_4 <- df_csmi_2015_lengthed_3 |> 
#   left_join(
#     csmi_lengths_species, by = c(
#       "species_group"="species_name")
#   ) |> 
#   mutate(seed = 1:nrow(df_csmi_2015_lengthed_3)+10) %>%
#   mutate(
#     length_mm = case_when(
#       compartment=="fishes" & is.na(length_mm) ~
#         pmap_dbl(list(mean, sd, seed), function(x, y, z){
#           set.seed(z); rnorm(1, x, y)
#         }
#         ),
#       TRUE ~ length_mm
#     )
#   ) |>
#   select(-mean, -sd, -seed) |>
#   mutate_all(~ifelse(is.nan(.), NA, .))  # fix NaN
# 
# df_csmi_2015_lengthed_4 |> filter(compartment=="fishes", is.na(length_mm))|> count() # 81
# # df_csmi_2015_lengthed_4 |> 
# #   # relocate(c(length_mm, mean, sd), .after = length_mm) |>
# #   filter(compartment=="fishes") |> View()
# 
# 
# # From 85% to ~10% no lengths
# saveRDS(df_csmi_2015_lengthed_4, here("out","data","df_csmi_2015_lengthed_v2.rds"))

# Load csmi data that has length data attributed to fishes
df_csmi_2015_lengthed_4 <- readRDS(here("out","data","df_csmi_2015_lengthed_v2.rds"))



#### Finalize data ------------------

df_csmi_2015_clean <- df_csmi_2015_lengthed_4 |>
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
    here("data-raw","raw_turschak_maitland_query.xlsx"), 
    sheet = "2002-2003 SIA Data") |> 
  cleans_names_and_caps()

# Variable selection and flags
df_uwm_2002_fish <- raw_uwm_2002_fish |> 
  filter(!flag==1) |>  # removes 15 rows with missing or bad data
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

# seasons
df_uwm_2002_fish <- df_uwm_2002_fish |> 
  mutate(
    date = lubridate::as_date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)

# Standardize columns
df_uwm_2002_fish <- df_uwm_2002_fish |> 
  mutate(
    dataset = "uwm_2002_2003",
    mass_g = NA, 
    num_ind = NA
    ) |>
  relocate_columns()


### UWM 2010 fish =============================================================


# Read data
raw_uwm_2010_fish <-
  read_xlsx(
    here("data-raw","raw_bootsma_2010_2011_fish.xlsx"),
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
  
# seasons
df_uwm_2010_fish <- df_uwm_2010_fish |> 
  mutate(
    date = lubridate::as_date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(year = ifelse(is.na(date), 2010, year)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)


# Standardize columns
df_uwm_2010_fish <- df_uwm_2010_fish |> 
  mutate(
    dataset = "uwm_2010_2011",
    compartment = "fishes", 
    species_group = species,
    num_ind = NA,
    length_mm = ifelse(length_mm == 99999, NA, length_mm),
    mass_g = NA, 
    # convert the text depth labels to numeric
    depth_m = case_when(
      depth_m == "shallow" ~ 3, 
      depth_m == "intermediate" ~ 8, 
      depth_m == "deep" ~ 15, 
      depth_m == "various" ~ as.double(NA)
    )
    ) |>
  relocate_columns()

# skim(df_uwm_2010_fish)

### UWM 2010 benthic =========================================================

# Notes: 
# - I filled in missing site data for much easier data cleaning
# - all zoops are 63 um mesh

# Read data
raw_uwm_2010_benthic <- read_xlsx(
  here(
    "data-raw",
    "raw_Nearshore seston and benthos stable isotopes 2010 for Bryan Maitland.xlsx"), 
  sheet = "Benthic", 
  skip = 6
  ) |>
  cleans_names_and_caps() 


# Variable selection and flags
df_uwm_2010_benthic <- raw_uwm_2010_benthic |> 
  filter(!flag==1) |>  # removed 20
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


# Deal with site/port names and standardize
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
df_uwm_2010_benthic <- df_uwm_2010_benthic |> 
  mutate(species = case_when(
    str_detect(species, "algae") ~ "benthic algae", 
    str_detect(species, "amphipod") ~ "amphipod", 
    str_detect(species, "chironomid") ~ "chironomids", 
    str_detect(species, "crayfish") ~ "crayfish", 
    str_detect(species, "cladophora") ~ "benthic algae", 
    str_detect(species, "dipteran") ~ "chironomids", 
    str_detect(species, "hydracarina") ~ "hydrachnidiae", 
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
      species %in% c("benthic algae") ~ "benthic algae", 
      species %in% c("dreissena") ~ "dreissenids", 
      is.na(species) ~ NA_character_, 
      TRUE ~ "benthic inverts"
  ))


# seasons
df_uwm_2010_benthic <- df_uwm_2010_benthic |> 
  mutate(
    date = lubridate::as_date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)

# Standardize 
df_uwm_2010_benthic <- df_uwm_2010_benthic |> 
  mutate(
    dataset = "uwm_2010_2011",
    length_mm = NA_integer_, 
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
    here("data-raw","raw_Kornis et al 2014 isotope data.xlsx"), 
    sheet = "Kornis et al. data spreadsheet"
  ) |> 
  cleans_names_and_caps()


# Variable selection
df_kornis_2014 <-raw_kornis_2014 |> 
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

# remove duplicates
df_kornis_2014 |> janitor::get_dupes(sample_id)
df_kornis_2014 <- df_kornis_2014 |> distinct(sample_id, .keep_all = TRUE)

# Fix regions / port names
df_kornis_2014 <- df_kornis_2014 |> 
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
df_kornis_2014 <- df_kornis_2014 |> 
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
      species == "dreissena" ~ "dreissenids", 
      species == "mysis" ~ "zooplankton", 
      TRUE ~ "fishes"
      )
  ) 

# seasons
df_kornis_2014 <- df_kornis_2014 |> 
  mutate(
    date = lubridate::as_date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)

# Standardize
df_kornis_2014 <- df_kornis_2014 |> 
    mutate(
      dataset = "kornis_2014",
      sample_id = as.character(sample_id),
      depth_m = NA,
      species_group = species, 
      num_ind = NA,
    ) |>
    relocate_columns()

# only keep salmonines for now 
# df_kornis_2014 <- df_kornis_2014 |> 
#   filter(species %in% c(
#     "brown trout", 
#     "chinook salmon", 
#     "coho salmon",
#     "rainbow trout", 
#     "lake trout"
#     ))
  


### NPS 2015 salmonids ========================================================

# Read data
raw_nps_2015_salmonids <- 
  read_csv(
    here("data-raw", "raw_Salmonine_Isotope_2015.csv")
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

df_nps_2015_salmonids |> janitor::get_dupes(sample_id)
df_nps_2015_salmonids <- df_nps_2015_salmonids |> distinct(sample_id, .keep_all = TRUE)

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


# seasons
df_nps_2015_salmonids <- df_nps_2015_salmonids |> 
  mutate(
    date = lubridate::mdy(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)

# Standardize
df_nps_2015_salmonids <- df_nps_2015_salmonids |> 
  mutate(
    dataset = "nps_2015",
    sample_id = as.character(sample_id),
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
    here("data-raw","raw_turschak_maitland_query.xlsx"), 
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

# dups
df_glft_2016_fish |> get_dupes(sample_id)


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


# seasons
df_glft_2016_fish <- df_glft_2016_fish |> 
  mutate(
    date = lubridate::as_date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)


# Standardize
df_glft_2016_fish <- df_glft_2016_fish |> 
  mutate(
    dataset = "glft_2016",
    mass_g = NA, 
    num_ind = NA,
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
  here("data-raw", "raw-Roth-2019.csv")
  ) |> 
  cleans_names_and_caps() 

# SIte metadata
xref_roth_ports <- 
  read_xlsx(here("data-raw","roth-2019-site-codes.xlsx")) |> 
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

# dups 
df_roth_2019_fish |> get_dupes(sample_id) 
df_roth_2019_fish <- df_roth_2019_fish |> distinct(sample_id, .keep_all = TRUE)

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


# seasons
df_roth_2019_fish <- df_roth_2019_fish |> 
  mutate(
    date = lubridate::as_date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(year = case_when(is.na(year) ~ 2019, TRUE ~ 2019)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)


# Standardize 
df_roth_2019_fish <- df_roth_2019_fish |> 
  select(-site_code) |>
  mutate(
    dataset = "roth_2019",
    # port = NA,
    compartment = case_when(
      species %in% c("oligo-chiro", "dreissena") ~ "benthic inverts", 
      species %in% c("dreissena") ~ "dressenids",
      TRUE ~ "fishes"),
    num_ind = NA,
    species_group = species, 
    ) |> 
  relocate_columns()


# Remove Lake Huron data
df_roth_2019_fish <- df_roth_2019_fish |> filter(lake_region != "Huron") 


### Oreilly 2014-2015 --------------------------------------------

# Read data
raw_oreilly_2014_15 <- read_csv(
  here("data-raw", "raw_O'Reilly_et_al_Nearshore_LakeMichigan_Isotope_Data.csv")
) |> 
  cleans_names_and_caps() 


# Variable selection
df_oreilly <- raw_oreilly_2014_15 |>
  select(
    sample_id, 
    date = collection_date_m_d_yr, 
    year,
    port = site, 
    species = taxa_species,
    length_mm = total_length_mm,
    d15n,  
    d13c, 
    cn = c_n
    )

df_oreilly |> get_dupes(sample_id)
df_oreilly <- df_oreilly |> distinct(sample_id, .keep_all = TRUE)

# Link to xrefs for lake regions
df_oreilly <- df_oreilly |> 
  left_join(xref_ports |> select(port, lake_region), by = "port") 


# taxa
df_oreilly <- df_oreilly |> 
  mutate(species = case_when(
    species == "amphipoda" ~ "amphipod",
    species == "chironomidae" ~ "chironomids",
    species == "dreissenidae" ~ "dreissena",
    species == "seston" ~ "pom",
    species == "steelhead" ~ "rainbow trout",
    species == "redhorse (moxostoma spp.)" ~ "shorthead redhorse", 
    TRUE ~ species
  )) 


# seasons
df_oreilly <- df_oreilly |> 
  mutate(
    date = lubridate::mdy(date),
    year = lubridate::year(date),
    month = lubridate::month(date)) |> 
  mutate(season = case_when(
    month %in% c(12,1,2) ~ "winter", 
    month %in% c(3,4,5) ~ "spring", 
    month %in% c(6,7,8) ~ "summer", 
    month %in% c(9,10,11) ~ "fall")) |> 
  select(-month)




# Standardize 
df_oreilly <- df_oreilly |> 
  mutate(
    dataset = "oreilly_2014_15",
    depth_m = NA,
    mass_g = NA,
    compartment = case_when(
      species %in% c("amphipod","chironomids","hydrachnidiae",
                     "gastropoda","sphaeriidae","trichoptera") ~ "benthic inverts", 
      species %in% c("dreissena") ~ "dreissenids",
      species %in% c("pom") ~ "pom",
      TRUE ~ "fishes"
      ),
    num_ind = NA,
    species_group = case_when(
      species == "pom" ~ paste(species, length_mm, sep = "_"), 
      TRUE ~ species
    ), 
    length_mm = case_when(
      length_mm %in% c("50 um","160 um","250 um") ~ NA, 
      length_mm == "nan" ~ NA,
      TRUE ~ length_mm
    ), 
  ) |>
  mutate(length_mm = as.numeric(length_mm)) |> 
  relocate_columns()



## Combine data ============================================================

data <- bind_rows(
  df_uwm_2002_fish, 
  df_uwm_2010_fish, 
  df_uwm_2010_benthic, 
  df_kornis_2014,
  df_oreilly,
  df_csmi_2015_clean,
  df_nps_2015_salmonids,
  df_glft_2016_fish,
  df_roth_2019_fish,
) 

# rm(df_uwm_2002_fish); rm(df_uwm_2010_fish); rm(df_uwm_2010_benthic)
# rm(df_kornis_2014); rm(df_csmi_2015_clean); rm(df_nps_2015_salmonids); rm(df_oreilly)
# rm(df_glft_2016_fish); rm(df_roth_2019_fish); rm(df_csmi_2015_lengthed)
# rm(list=ls(pattern="^raw")); rm(df_csmi_2015)
# rm(csmi_2015_spp_xref); rm(csmi_lengths); rm(csmi_lengths_dists)


## Clean taxonomy ===========================================================

# check taxonomy
data |> count(compartment)
data |> count(compartment, species) |> print(n=Inf)

data <- data |> 
  mutate(
    species = case_when(
      species == "algae" ~ "benthic algae",
      species == "bloater ip" ~ "bloater",
      species == "alewife ip" ~ "alewife",
      species == "deepwater sculpin ip" ~ "deepwater sculpin",
      species == "offshore round goby" ~ "round goby", 
      TRUE ~ species
      ), 
    species_group = case_when(
      species_group == "algae" ~ "benthic algae", 
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
  left_join(xref_sci_names |> select(-compartment), by = c("species"="common_name")) |> 
  relocate(sci_name, .before = species)


# check taxonomy
# data |> count(compartment)
# data |> count(compartment, species) |> print(n=Inf)
# data |> count(compartment, sci_name) |> print(n=Inf)
# data |> count(species_group) |> print(n=Inf)

rm(list=ls(pattern="^xref"))


# Assign factors =========================================================

data <- data |> 
  select(-num_ind) |>
  mutate(
    depth_g = case_when(
      dataset == "oreilly_2014_15" ~ "ns",
      depth_m <= 30 ~ "ns", 
      depth_m > 30 & depth_m <= 100 ~ "of-d", 
      depth_m > 100 ~ "of-p", 
      TRUE ~ "unkn"
    )
  ) |>
  relocate(depth_g, .after = depth_m) |> 
  mutate(
    dataset = factor(
      dataset, 
      levels = c(
        "uwm_2002_2003","uwm_2010_2011","kornis_2014", "oreilly_2014_15",
        "csmi_2015","nps_2015","glft_2016","roth_2019"
      )), 
    compartment = factor(
      compartment, levels = c(
        "pom", "benthic algae","benthic inverts",
        "dreissenids","ichthoplankton","zooplankton","fishes"
      )), 
    lake_region = factor(
      lake_region, 
      levels = c("nw", "ne", "sw", "se")), 
    season = factor(season, levels = c("winter","spring","summer","fall")), 
    depth_g = factor(depth_g, 
                     levels = c("ns","os-d","os-p","unkn"))
  ) |> 
  mutate(across(where(is.character), as.factor)) 



## Lipid normalization ========================================================

data <- data |>
  mutate(
    d13c_norm = case_when(
      # non-zoop inverts = Logan et al. (2008)
      compartment %in% c("benthic inverts","dreissenids") & cn > 3.25 ~ 
        0.967 * d13c + 0.861,
      # zoops = Smyntek et al. (2007)
      compartment == "zooplankton" ~ 
        d13c + (6.3 * ((cn - 4.2)/(cn))),
      # fishes = Hoffman et al. (2015)
      compartment %in% c("fishes", "ichthoplankton") & cn > 3.25 ~
        d13c + (-6.5 * (3.5 - cn)) / cn,
      TRUE ~ d13c
    )
  ) |> 
  relocate(d13c_norm, .after = d13c)

## Save compiled data ==========================================================

skim(data)

save(data, file = here("out", "data", "compiled_data.RData"))
# load(here("out", "data", "compiled_data_v3.RData"))


