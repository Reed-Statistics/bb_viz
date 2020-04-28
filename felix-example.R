library(pitchRx)
library(baseballr)
library(tidyverse)
library(viridis)
library(scales)
library(plotly)

`%notin%` <- Negate(`%in%`)


# Grabs pitch data from a season
# Can only grab a season at once
felix <- scrape_statcast_savant(start_date = "2014-03-20",
                                end_date = "2014-11-10",
                                playerid = 433587,
                                player_type = "pitcher")

felix <- felix %>%
  mutate(description = ifelse(description == "blocked_ball", "Blocked Ball",
                              ifelse(description == "called_strike", "Called Strike",
                                     ifelse(description %in% c("ball",
                                                               "intent_ball"), "Ball",
                                            ifelse(description == "foul", "Foul",
                                                   ifelse(description %in% c("swinging_strike",
                                                                           "swinging_strike_blocked"), "Swinging Strike",
                                                          ifelse(description %in% c("hit_into_play_no_out",
                                                                                    "hit_into_play_score",
                                                                                    "hit_into_play"), "Hit into Play",
                                                                 ifelse(description == "pitchout", "Pitch Out",
                                                                        ifelse(description == "foul tip", "Foul Tip",
                                                                               ifelse(description %in% c("missed_bunt",
                                                                                                         "foul_bunt"), "Bunt Attempt", "Hit by Pitch")))))))))) %>%
  mutate(pitch_type = ifelse(pitch_type == "CU", "Curveball",
                              ifelse(pitch_type == "SI", "Sinker",
                                     ifelse(pitch_type == "CH", "Changeup",
                                            ifelse(pitch_type == "FF", "Fastball",
                                                   ifelse(pitch_type == "PO", "Pitch out",
                                                          ifelse(pitch_type == "SL", "Slider",
                                                                 ifelse(pitch_type == "IN", "Intentional Ball", "Null"))))))))

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


# Plotting by pitch type with interactivity

felix_2014 <- felix %>%
  filter(pitch_type %notin% c("IN", "null")) %>%
  ggplot(mapping = aes(x = plate_x,
                     y = plate_z,
                     color = pitch_type,
                     text = paste('Date: ', game_date, "\n",
                                  'Pitch: ', pitch_type, "\n",
                                  'Release Speed: ', release_speed, "\n",
                                  'Result: ', description, "\n",
                                  sep = "")
                     )
         ) +
  coord_fixed() +
  geom_point(alpha = 0.5) +
  geom_segment(x = -0.85, xend = 0.85, y = 3.5, yend = 3.5, size = 0.7, color = "black", lineend = "round") +
  geom_segment(x = -0.85, xend = 0.85, y = 1.5, yend = 1.5, size = 0.7, color = "black", lineend = "round") +
  geom_segment(x = -0.85, xend = -0.85, y = 1.5, yend = 3.5, size = 0.7, color = "black", lineend = "round") +
  geom_segment(x = 0.85, xend = 0.85, y = 1.5, yend = 3.5, size = 0.7, color = "black", lineend = "round") +
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

felix_2014


ggplotly(felix_2014, dynamicTicks = TRUE, tooltip = 'text') %>%
  layout(xaxis = list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE), 
    yaxis = list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE))
