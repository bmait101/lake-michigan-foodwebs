# Site and species lists

# Sites (from definitions tab of raw data file)
site_ref <- tibble(
  site_code = c(
    "ARC", "LUD", "MAN", "MID", "RAC", "LARS", "SAU", "StB", "StJ", "WAK", "Z.unk"), 
  site_name = c(
    "Frankfort", "Ludington", "Manitowoc", "Midlake",  "Racine", "Deep Station", 
    "Saugatuk", "Sturgeon Bay", "Saint Joes", "Waukegan", "Unknown")
  )

# Species 
spp_ref <- tibble(
  spp_code = c(
    "AIP","ALE","ALG","AMP","BIP","BLO","BUT","BYT","CHR","CRA","DIP","DWS","HEM",
    "LTR","LWF","MUS","MYS","OLG","POM","RAS","ROG","SMS","SPS","WHS","YEP","ZOP",
    "ZOP153","ZOP63"
  ), 
  spp_name = c(
    "Alewife IP", "Alewife", "Algae", "Amphipod", "Bloater IP", "Bloater", "Burbot", 
    "Bythotrephes", "Chironomics", "Crayfish", "Diporeia", "DW Sculpin", "Hemimysis", 
    "Lake Trout", "Lake WF", "Dreissenid", "Mysis", "Oligochaete", "POM", "Rainbow Smelt", 
    "Round Goby", "Slimy Sculpin", "Spottail Shiner", "White Sucker", "Yellow Perch", 
    "ZOP", "ZOP153", "ZOP63"
  )
)
