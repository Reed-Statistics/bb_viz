library(pitchRx)
library(baseballr)
library(tidyverse)
library(viridis)
library(scales)

`%notin%` <- Negate(`%in%`)


# Grabs pitch data from a season
# Can only grab a season at once
felix <- scrape_statcast_savant(start_date = "2014-03-20",
                                end_date = "2014-11-10",
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
  xlim(-6,6) +
  labs(color = "Release Speed",
       title = "Felix Hernandez Pitches by Release Speed",
       subtitle = "2014 MLB Season") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey96"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        legend.position = "bottom") +
  guides(colour = guide_colourbar(title.position = "top"))


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
  labs(color = "Pitch Type",
       title = "Felix Hernandez Pitches by Pitch Type",
       subtitle = "2014 MLB Season") +
  xlim(-6,6) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey96"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        legend.position = "bottom") +
  guides(colour = guide_legend(title.position = "top"))
