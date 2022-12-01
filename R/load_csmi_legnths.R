# read in csmi length data and prep for use


# Read CSMI lengths data for trawled fish
csmi_lengths <-
  read_xlsx(
    here("data","CSMI lengths from trawled fishes.xlsx"), 
    sheet = "Export Worksheet"
  ) |> 
  cleans_names_and_caps()

# Prepare length data for summarizing species length distributions
csmi_lengths <- csmi_lengths |> 
  filter(is.na(added_to_raw_csmi)) |> # remove the individuals I manually inputed
  filter(! species_name %in% c(
    "channel catfish", "coho salmon","freshwater drum",
    "gizzard shad", "burbot","spottail shiner","longnose sucker", 
    "ninespine stickleback", "trout perch", "white sucker", "rainbow smelt")) |> 
  mutate(
    port_name = ifelse(port_name=="frankfort", "arcadia", port_name),
    date = lubridate::as_date(op_date),
    month = lubridate::month(date),
    year = lubridate::year(date)) |>
  mutate(season = case_when(
    month %in% c(1,2,3,4,5) ~ 1, 
    month %in% c(6,7) ~ 2, 
    month %in% c(8,9,10,11) ~ 3,
    TRUE ~ 4
  )) |> 
  mutate(season = as.character(season)) |> 
  select(-op_date, -serial, -port) |> 
  relocate(season, .before = port_name) |> 
  # Assign specific length groups based on CSMI guidance for SIA fish
  mutate(species_name = case_when(
    species_name == "alewife" & length >= 100 ~ "alewife lg",
    species_name == "alewife" & length < 100 ~ "alewife sm",
    species_name == "bloater" & length >= 120 ~ "bloater lg",
    species_name == "bloater" & length < 120 ~ "bloater sm",
    species_name == "slimy sculpin" & length >= 40 ~ "slimy sculpin lg",
    species_name == "slimy sculpin" & length < 40 ~ "slimy sculpin sm", 
    TRUE ~ species_name
  ))
