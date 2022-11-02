# Loan and prep datasets

# libraries
pacman::p_load(here, readr, readxl, dplyr, tidyr, ggplot2, janitor)


## CSMI 2015 ==================================================

# Load data
# provided as an xlsx file, I extracted metadata separately
raw_csmi2015 <- readxl::read_xlsx(
  here("data","raw-CSMI-2015.xlsx"), 
  sheet = "Combined UF MED"
  )

# load xref tables I made from metadata
xref_csmi_sites <- read.csv(here("data", "xref-csmi-sites.csv"))
xref_csmi_spp <- read.csv(here("data", "xref-csmi-spp.csv"))


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
    site_code = Site,         # 3-letter site codes codes
    season = Season,          # 1=may, 2=june/july, 3=aug/sep
    depth_m = `Depth(m)`,     # depth in meters
    spp_code = Species_O,    # detailed species subgroup linked to my xref table
    d13C = d13Cb,     # bulk d13C
    # d13C_c1 = d13Cc,   # Lipid corrected values via Hoffman
    # d13C_c2 = d13Cc2,  # Lipid corrected values: inverts-Smynek, fish-Hoffman
    cn = `C:N`,
    )


# make explicit some data
df_csmi <- df_csmi |> 
  mutate(
    sample_type = case_when(
      sample_type == "F" ~ "fish", 
      sample_type == "I" ~ "invert", 
      TRUE ~ NA_character_
      ), 
    # season = case_when(
    #   season == 1 ~ "May", 
    #   season == 2 ~ "Jun/Jul", 
    #   season == 3 ~ "Aug/Sep", 
    #   TRUE ~ NA_character_
    #   ), 
    spp_code = ifelse(
      spp_code=="LTR", "LAT", spp_code
      )
    )

# merge xref tables with data
df_csmi <- df_csmi |> 
  left_join(xref_csmi_sites, by = "site_code") |> 
  left_join(xref_csmi_spp, by = "spp_code")

# Clean up
df_csmi <- df_csmi |> 
  relocate(d13C, .after = d15N) |>
  relocate(c(d15N, d13C, cn), .after = sample_id) |> 
  relocate(species, .after = spp_code) |> 
  mutate(sample_type = coalesce(sample_type, "POM")) |> 
  mutate(sample_type = ifelse(spp_code=="ALG", "Algae", sample_type))


## Missing data
# df_csmi %>%
#   gather(key = "key", value = "val") %>%
#   mutate(is.missing = is.na(val)) %>%
#   group_by(key, is.missing) %>%
#   summarise(num.missing = n()) %>%
#   filter(is.missing==T) %>%
#   select(-is.missing) %>%
#   arrange(desc(num.missing)) |> 
#   print()

df_csmi <- df_csmi |> 
  drop_na(d15N, d13C, spp_code, depth_m, site_code, season) |> 
  filter(! site_name %in% c("Z.unk", "Unknown", "Midlake", "Deep Station")) |>
  filter(! season %in% c("NR")) |>
  droplevels()  


## Duplicate samples 
dups_csmi <- df_csmi |> 
  janitor::get_dupes(sample_id, spp_code, site_code, season, depth_m)
# dups_csmi |>  View()
# cannot tell because samples ids not really unique - so not removing any


## Extreme values
# df_csmi |>
#   ggplot(aes(d13C)) +
#   geom_histogram()

df_csmi <- df_csmi |> 
  filter(d13C > -50) |> 
  filter(sample_id != "5720") |> 
  filter(!(sample_id == "5688" & species == "POM")) 


## Subset depths
# df_csmi <- df_csmi |> 
#   filter(! depth_m %in% c("0","105", "91", "168", "272")) 
df_csmi <- df_csmi |> 
  filter(depth_m %in% c("2", "18", "46", "91", "110")) |> 
  mutate(depth_m = ifelse(depth_m=="91","110",depth_m)) |> 
  mutate(depth_m = factor(depth_m, levels = c("2", "18", "46", "110")))



## Lipid normalization
df_csmi <- df_csmi |> 
  mutate(
    d13C_norm = case_when(
      sample_type == "fish" ~ d13C + (3.5 * (3.5 - cn)) / cn, 
      # sample_type == "invert" ~ d13C + (3.5 * (3.5 - cn)) / cn,
      TRUE ~ d13C
    )
  )





## Roth 2019 =======================

# Data 
raw_roth19 <- readr::read_csv(here("data", "raw-Roth-2019.csv"))

# xref tables
xref_roth_spp <- read_csv(here("data", "xref-roth-spp.csv"))


## Clean / Tidy
df_roth <- raw_roth19 |> 
  select(
    -1,     # unknown column, no metadata
    -ID1, -ID2, -`SIF ID`,-`Serial/Operation`,-ORGANIZATION, 
    -Comments.x,-Comments.y, -`Box Number`, -`Box Location`, 
    -DryStart, -DryEnd, -QAQC, -TLin, -`Wt% N*`,-`Wt% C*`, -CAP_BASIN,
    -`PRED/PREY`, -`Size class`, -`Depth class`, -Stat_district, -Source
  )

df_roth <- df_roth |> 
  rename(
    sample_id = MSU_ID, 
    date = Date,       
    depth_m = `Depth (m)`,   
    site_no = CAP_SITE, 
    area = Region,
    spp_code = Species,
    d13C = Del13C,  
    d15N = Del15N, 
    cn = `C:N ratio`,
    length_mm = TLmm, 
    weight_g = TWg, 
  ) 

# Add species data from xref
df_roth <- df_roth |> 
  # left_join(xref_csmi_sites |> select(site_code, site_name), by = "site_code") |> 
  left_join(xref_roth_spp, by = "spp_code") |> 
  relocate(species, .after = spp_code)|> 
  relocate(c(length_mm, weight_g), .after = cn) 


## Duplicates samples
df_roth |> janitor::get_dupes(sample_id)  # n=66

df_roth <- df_roth |> 
  group_by(sample_id) |> 
  slice_head(n = 1) |> 
  ungroup()


## Missing data
# df_roth_missing <- df_roth %>%
#   gather(key = "key", value = "val") %>%
#   mutate(is.missing = is.na(val)) %>%
#   group_by(key, is.missing) %>%
#   summarise(num.missing = n()) %>%
#   filter(is.missing==T) %>%
#   select(-is.missing) %>%
#   arrange(desc(num.missing))

# df_roth_missing %>%
#   ggplot() +
#   geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
#   labs(x='variable', y="number of missing values", title='Number of missing values') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Drop missing data
df_roth <- df_roth |> 
  drop_na(d15N, d13C, area) |> 
  droplevels() 

levels(as.factor(df_roth$area))

## Remove Lake Huron data, ETC. 
df_roth <- df_roth |> 
  filter(! area %in% c("NH", "CH", "SH")) |> 
  filter(! species %in% c("Invertebrate")) |> 
  droplevels() 


## Lipid normalization
df_roth <- df_roth |> 
  mutate(
    d13C_norm = case_when(
      species != "Dreissenids" | species != "Invertebrate" ~ 
        d13C + (3.5 * (3.5 - cn)) / cn, 
      TRUE ~ d13C
    )
  )


