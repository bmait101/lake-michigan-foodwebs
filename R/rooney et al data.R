library(tidyverse)
library(patchwork)

df <- tibble(
  tp = c(
    1, 2.1, 2.2, 2.7, 3.1, 2.7, 2.8, 3.7, 3.7, 3.8, 4.4, 4.6, 
    2.1, 2.1, 2.8, 2.9, 3.5, 3.5, 3.6, 3.8, 4.1, 4.2, 4.3, 4.4, 4.6, 4.7
    ), 
  size = c(
    -5, -4, -3.8, -2.3, -2.4, 2, 0.8, 2.2, 2.3, 2.1, 2.5, 4.5, 
    -1.8, 2.2, 0.4, 1, 0.7, 2.3, 3.2, 1.9, 2.6, 1.7, 2.7, 1.6, 2.6, 2.7
    ),
  group  = c(
    "phyto","phyto","phyto","phyto","phyto","phyto","phyto","phyto","phyto","phyto",
    "phyto","phyto",
    "detrital","detrital","detrital","detrital","detrital","detrital","detrital",
    "detrital","detrital","detrital","detrital","detrital","detrital","detrital"
  )
)

df

p1 <- df |> 
  ggplot(aes(tp, size, group = group, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

p2 <- df |> 
  ggplot(aes(size, tp, group = group, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

p1 | p2
