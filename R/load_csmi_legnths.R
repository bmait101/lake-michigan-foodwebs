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


# Summarize size distributions for each species, port, season
csmi_lengths_dists <- csmi_lengths |> 
  group_by(species_name, season, port_name, station_depth) |> 
  summarise(
    mean = mean(length, na.rm = TRUE), 
    sd = sd(length, na.rm = TRUE), 
    .groups = 'drop'
  )



# Visualize length distributions
# csmi_lengths |> 
#   ggplot(aes(length)) + 
#   geom_density() + 
#   facet_grid(rows = vars(port_name), cols = vars(species_name), 
#              scales = 'free')

# Test it
# # Subset fish with no lengths for testing (only have length data for >18m)
# df_csmi_2015_nolen <- df_csmi_2015 |>
#   filter(is.na(length_mm), depth_m >= 18, compartment=="fishes") |>
#   select(species_group, season, port, depth_m)
#
# # Join distribution summary stats to csmi data
# df_csmi_2015_nolen_dists <- 
#   df_csmi_2015_nolen |> 
#   left_join(
#     csmi_lengths_dists, by = c(
#       "species_group"="species_name", 
#       "season", 
#       "port"="port_name", 
#       "depth_m"="station_depth")
#   )
#
# # Simulate lengths - using distribution stats
# df_csmi_2015_nolen_dists_lengthed <- 
#   df_csmi_2015_nolen_dists |> 
#   mutate(seed = 1:nrow(df_csmi_2015_nolen_dists)) %>% 
#   mutate(
#     length_mm = pmap_dbl(
#       list(mean, sd, seed), 
#       function(x, y, z){
#         set.seed(z); rnorm(1, x, y)
#       }
#     )
#   )
#
# Visualize simulated length distributions
# df_csmi_2015_nolen_dists_lengthed |>
#   ggplot(aes(length_mm)) +
#   geom_density() +
#   # geom_density(aes(color = factor(station_depth))) +
#   facet_grid(rows = vars(port), cols = vars(species_group),
#              scales = 'free')

