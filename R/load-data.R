# Loan and prep datasets

# libraries
pacman::p_load(here, tidyverse, readxl, janitor)

xref_spp <- read.csv(here("data", "xref-spp.csv"))

## CSMI 2015 ==================================================

# Load data
# provided as an xlsx file, I extracted metadata separately
raw_csmi2015 <- readxl::read_xlsx(
  here("data","raw-CSMI-2015.xlsx"), 
  sheet = "Combined UF MED"
  )

# load xref tables I made from metadata
# xref_csmi_sites <- read.csv(here("data", "xref-csmi-sites.csv"))


## Initial clean

# Get rid of columns
df_csmi <- raw_csmi2015 |> 
  select(
    -Depth_O,     # unknown column, no metadata
    -Tray,        # sample loading tray for isotope analysis 
    -Lab,         # ID of SI lab: EPA Duluth Minnesota (MED), U of Florida (UF)
    -Line,        # column from SI lab
    -`OP Notes`,  # comments from SI lab
    -Comments,    # general comments from lab,
    -`wt%N`,
    -`wt%C`, 
    -d13Cc, # Lipid corrected values via Hoffman
    -d13Cc2, # Lipid corrected values: inverts-Smynek, fish-Hoffman
    -Species
    )

# Fix some names
df_csmi <- df_csmi |> 
  rename(
    sample_id = `Sample ID`, 
    sample_type = `F/I`,      # f=fish, i=invert, z=standard
    site = Site,         # 3-letter site codes codes
    season = Season,          # 1=may, 2=june/july, 3=aug/sep
    depth = `Depth(m)`,     # depth in meters
    spp_code = Species_O,    # detailed species subgroup linked to my xref table
    d13C = d13Cb,     # bulk d13C
    # d13C_c1 = d13Cc,   # Lipid corrected values via Hoffman
    # d13C_c2 = d13Cc2,  # Lipid corrected values: inverts-Smynek, fish-Hoffman
    cn = `C:N`,
    )

# Add xref
df_csmi <- df_csmi |> 
  mutate(
    dataset = "csmi_2015",
    length_mm = NA, 
    year = 2015,  
    area = case_when(
      site %in% c("ARC", "LUD") ~ "NEM", 
      site %in% c("SAU", "StJ") ~ "SEM", 
      site %in% c("StB", "MAN") ~ "NWM", 
      site %in% c("RAC", "WAK") ~ "SWM"
      ),
    spp_code = ifelse(spp_code=="LTR", "LAT", spp_code),
    sample_type = case_when(
      sample_type == "F" ~ "fish", 
      sample_type == "I" ~ "invert", 
      TRUE ~ NA_character_
      ), 
    season = case_when(
      season == "1" ~ "Spring", 
      season == "2" ~ "Summer", 
      season == "3" ~ "Fall", 
      TRUE ~ season)
    ) |> 
  left_join(xref_spp, by = "spp_code") 


# Clean up
df_csmi <- df_csmi |> 
  relocate(dataset, .before = sample_id) |>
  relocate(c(spp_code, species), .after = sample_id) |>
  relocate(functional_group, .after = species) |>
  relocate(year, .after = functional_group) |>
  relocate(season, .after = year) |> 
  relocate(c(area,site), .after = season) |> 
  relocate(depth, .after = site) |> 
  mutate(sample_type = coalesce(sample_type, "POM")) 
# |> 
#   mutate(sample_type = ifelse(spp_code=="ALG", "Algae", sample_type))


# Missing data
df_csmi <- df_csmi |> 
  drop_na(d15N, d13C, spp_code, depth, site, season) |> 
  filter(! site %in% c("0", "Z.unk", "MID", "LARS")) |>
  filter(! season %in% c("NR")) |>
  droplevels()  

## Duplicates 
# dups_csmi <- df_csmi |> 
#   janitor::get_dupes(sample_id, spp_code, site, season, depth)
# dups_csmi |>  View()
# cannot tell because samples ids not really unique - so not removing any


## Filters
df_csmi <- df_csmi |> 
  filter(d13C > -50) |> 
  filter(sample_id != "5720") |> 
  filter(!(sample_id == "5688" & species == "POM")) |> 
  filter(depth %in% c("2", "18", "46", "91", "110")) |> 
  mutate(depth = ifelse(depth=="91","110",depth)) 

## Lipid normalization
# df_csmi <- df_csmi |> 
#   mutate(
#     d13C_norm = case_when(
#       sample_type == "fish" ~ d13C + (3.5 * (3.5 - cn)) / cn, 
#       # sample_type == "invert" ~ d13C + (3.5 * (3.5 - cn)) / cn,
#       TRUE ~ d13C
#     )
#   )


## Roth 2019 =======================

# Data 
raw_roth19 <- readr::read_csv(here("data", "raw-Roth-2019.csv"))

## Clean / Tidy
df_roth <- raw_roth19 |> 
  select(
    -1,     # unknown column, no metadata
    -ID1, -ID2, -`SIF ID`,-`Serial/Operation`,-ORGANIZATION, 
    -Comments.x,-Comments.y, -`Box Number`, -`Box Location`, 
    -DryStart, -DryEnd, -QAQC, -TLin, -`Wt% N*`,-`Wt% C*`, -CAP_BASIN,
    -`PRED/PREY`, -`Size class`, -`Depth class`, -Stat_district, -Source
  ) |> 
  rename(
    sample_id = MSU_ID, 
    date = Date,       
    depth = `Depth (m)`,   
    site = CAP_SITE, 
    area = Region,
    spp_code = Species,
    d13C = Del13C,  
    d15N = Del15N, 
    cn = `C:N ratio`,
    length_mm = TLmm, 
    weight_g = TWg, 
  ) 

# sample tyep and year and season from date
df_roth <- df_roth |> 
  mutate(
    depth = as.character(depth),
    dataset = "roth_2019",
    sample_type = case_when(
      spp_code %in% c("DRE", "INT") ~ "invert", 
      TRUE ~ "fish"), 
    year = lubridate::year(date), 
    season = as.character(lubridate::quarter(date, with_year = FALSE, fiscal_start = 1))
  ) |> 
  mutate(
    season = case_when(
      season == "1" ~ "Spring", 
      season == "2" ~ "Summer", 
      season == "3" ~ "Fall", 
      TRUE ~ season)) |> 
  relocate(c(year, season), .after = date) |> 
  select(-date)

# Add xrefs
df_roth <- df_roth |> 
  mutate(
    spp_code = case_when(
      spp_code=="DRE" ~ "MUS", 
      spp_code=="SMT" ~ "RAS", 
      spp_code=="RGB" ~ "ROG", 
      TRUE ~ spp_code)) |> 
  left_join(xref_spp, by = "spp_code") |> 
  relocate(dataset, .before = sample_id) |>
  relocate(c(spp_code, species, functional_group), .after = sample_id) |> 
  relocate(c(length_mm, weight_g), .after = cn) |> 
  relocate(c(area), .before = site) |> 
  relocate(c(d15N, d13C, cn, length_mm, weight_g), .after = sample_type) 
  


## Duplicates samples
df_roth |> janitor::get_dupes(sample_id)  # n=66

df_roth <- df_roth |> 
  group_by(sample_id) |> 
  slice_head(n = 1) |> 
  ungroup()


# Drop missing data
df_roth <- df_roth |> 
  drop_na(d15N, d13C, area) |> 
  droplevels() 


## Remove Lake Huron data, ETC. 
df_roth <- df_roth |> 
  filter(! area %in% c("NH", "CH", "SH")) |> 
  filter(! species %in% c("Invertebrate")) |> 
  droplevels() 



## Happel 2010 ==========================

raw_happel2010 <- readxl::read_xlsx(
  here("data","raw-feiner-2018.xlsx"), 
  sheet = "Diet-StableIsotope"
)

## Clean 
df_happ <- raw_happel2010 |> 
  janitor::clean_names() |> 
  select(
    sample_id = fish_id, spp_code = species, year, season, site, depth, 
    d15N = corrected_d29_28,  # Corrected δ15N nitrogen stable isotope ratios
    d13C = corrected_d13_12,  # Corrected δ13C carbon stable isotope ratios
    # corr_c,  # Lipid-corrected δ13C carbon isotopic ratio (using corrections in Turschak et al. 2014)
    # corr_n, # Corrected δ15N
    length_mm = tl
    ) |> 
  mutate(
    dataset = "happel_2010",
    spp_code = ifelse(spp_code=="STS", "SPS", spp_code), 
    sample_type = "fish", 
    length_mm = as.double(length_mm),
    cn = NA,
    area = case_when(
      site %in% c("5R") ~ "NEM", 
      site %in% c("3S", "4R", "4S") ~ "SEM", 
      site %in% c("6R") ~ "NWM", 
      site %in% c("1R", "1S", "2R", "2S", "3R") ~ "SWM")) |> 
  left_join(xref_spp, by = "spp_code") |> 
  relocate(dataset, .before = sample_id) |>
  relocate(c(species, functional_group), .after = spp_code) |> 
  relocate(c(sample_type), .after = depth) |> 
  relocate(c(area), .before = site) |> 
  relocate(c(cn), .before = length_mm)

# Missing data and filters
df_happ <- df_happ |> 
  # drop_na(depth) |> 
  filter(! depth %in% c("unk", "var")) |>
  droplevels()  




## Combine data =====================

# head(df_csmi)
# head(df_roth)
# head(df_happ)

data <- bind_rows(df_csmi, df_roth, df_happ)
