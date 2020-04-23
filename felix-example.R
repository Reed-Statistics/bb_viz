library(pitchRx)
library(baseballr)
library(tidyverse)
library(viridis)

`%notin%` <- Negate(`%in%`)


# Grabs pitch data from start of dataset to today
felix <- scrape_statcast_savant(start_date = "2008-03-25",
                                 end_date = Sys.Date(),
                                 playerid = 433587,
                                 player_type = "pitcher")

# Plots place ball crosses the plate with color mapped to release speed
ggplot(data = felix,
       mapping = aes(x = plate_x,
                     y = plate_z,
                     color = release_speed)) +
  geom_point(alpha = 0.5) +
  geom_rect(mapping = aes(ymax = 3.5, ymin = 1.5, 
                          xmax = -0.85, xmin = 0.85), alpha = 0, size=1.2,
            colour = "black") +
  scale_color_viridis_c() +
  theme_bw()

# Plotting by pitch type
felix %>%
  filter(pitch_type %notin% c("IN", "null")) %>%
  ggplot(mapping = aes(x = plate_x,
                     y = plate_z,
                     color = pitch_type)) +
  geom_point(alpha = 0.5) +
  geom_rect(mapping = aes(ymax = 3.5, ymin = 1.5, 
                          xmax = -0.85, xmin = 0.85), alpha = 0, size=1.2,
            colour = "black") +
  scale_color_viridis_d() +
  theme_bw() +
  xlim(-10,10) +
  ylim(-1,7)
