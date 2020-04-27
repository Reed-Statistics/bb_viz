## Libraries

library(tidyverse)
library(baseballr)


## Player ID

players <- players


## Pull Data

arenado <- scrape_statcast_savant(
  start_date = "2019-03-28",
  end_date = "2019-09-27",
  playerid = 571448,
  player_type = "batter")


## Wrangle

arenado <- arenado %>%
  filter(description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score")) %>%
  mutate(hit_result = ifelse(events == "single", "Single",
                                    ifelse(events == "double", "Double",
                                           ifelse(events == "triple", "Triple",
                                                  ifelse(events == "home_run", "Home Run",
                                                         ifelse(events == "sac_fly", "Sacrifice Fly", "Out"))))))
                                                 


## Sample Plot (Spray Chart)

arenado %>%
  ggplot(aes(x = hc_x, y = -hc_y)) +
  geom_segment(x = 128, xend = 20, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  geom_segment(x = 128, xend = 236, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  geom_curve(x = 77, xend = 178, y = -155, yend = -156,
             curvature = -0.65, linetype = "dotted", color = "grey66") +
  coord_fixed() +
  geom_point(aes(color = hit_result), alpha = 0.75, size = 2) +
  scale_x_continuous(limits = c(25, 225)) +
  scale_y_continuous(limits = c(-225, -25)) +
  labs(title = "Nolan Arenado Spray Chart",
       subtitle = "2019 MLB Season",
       color = "Hit Result") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey96"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

  
## Sample Plot (w / Density)

arenado %>%
  ggplot(aes(x = hc_x, y = -hc_y)) +
  geom_segment(x = 128, xend = 18, y = -208, yend = -100, size = 0.7, color = "grey50", lineend = "round") +
  geom_segment(x = 128, xend = 238, y = -208, yend = -100, size = 0.7, color = "grey50", lineend = "round") +
  geom_curve(x = 76, xend = 179, y = -155, yend = -156,
             curvature = -0.65, linetype = "dotted", color = "grey 50") +
  coord_fixed() +
  geom_point(aes(color = hit_result), alpha = 0.75, size = 2) +
  geom_density_2d(size = 1, alpha = 0.5) +
  scale_x_continuous(limits = c(25, 225)) +
  scale_y_continuous(limits = c(-225, -25)) +
  labs(title = "Nolan Arenado Spray Chart",
       subtitle = "2019 MLB Season",
       color = "Hit Result") +
  theme_void() + theme(plot.background = element_rect(fill = "grey95")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))


## Shiny App Spray Chart Idea:

# color by: appropriate factor colors (i.e. out = red)
# input: player name
# input: date range
# color by options: hit result, launch angle, exit velocity, estimated BA
# filter by options: hit result
# ouput: plot, title, summary stats
# toggle option: density overlay
# warning: comtains missing values
