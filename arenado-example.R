## Libraries

library(tidyverse)
library(baseballr)
library(plotly)


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
                                                         ifelse(events == "sac_fly", "Sacrifice Fly", "Out")))))) %>%
  mutate(hit_type = ifelse(bb_type == "line_drive", "Line Drive",
                             ifelse(bb_type == "fly_ball", "Fly Ball",
                                    ifelse(bb_type == "ground_ball", "Ground Ball", "Pop Fly")))) %>%
  mutate(hit_result = fct_relevel(hit_result, c("Out", "Sacrifice Fly", "Single", "Double", "Triple", "Home Run"))) 
                                                 


## Sample Plot (Spray Chart)

arenado_plot <- arenado %>%
  ggplot(aes(x = hc_x, y = -hc_y)) +
  geom_segment(x = 128, xend = 20, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  geom_segment(x = 128, xend = 236, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  geom_curve(x = 77, xend = 178, y = -155, yend = -156,
             curvature = -0.65, linetype = "dotted", color = "grey66") +
  coord_fixed() +
  geom_point(aes(color = hit_result), alpha = 0.6, size = 2) +
  scale_x_continuous(limits = c(25, 225)) +
  scale_y_continuous(limits = c(-225, -25)) +
  labs(title = "Nolan Arenado Spray Chart",
       subtitle = "2019 MLB Season",
       color = "Hit Result") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey96"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))
arenado_plot


### Sample Plot (Interactive Spray Chart)

arenado_plot2 <- arenado %>%
  ggplot(aes(x = hc_x, y = -hc_y,
             text = paste('Date: ', game_date, "\n",
                          'Pitch: ', pitch_name, "\n",
                          'Hit Type: ', hit_type, "\n",
                          'Hit Result: ', hit_result, "\n",
                          'Exit Velocity (MPH): ', launch_speed, "\n",
                          'Launch Angle: ', launch_angle, "\n",
                          'Estimated Distance (ft): ', hit_distance_sc, "\n",
                          sep = ""))) +
  geom_segment(x = 128, xend = 20, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  geom_segment(x = 128, xend = 236, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  coord_fixed() +
  geom_point(aes(color = hit_result), alpha = 0.6, size = 2) +
  scale_x_continuous(limits = c(25, 225)) +
  scale_y_continuous(limits = c(-225, -25)) +
  labs(title = "Nolan Arenado Spray Chart",
       subtitle = "2019 MLB Season") +
  theme_void() +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank())

ggplotly(arenado_plot2, dynamicTicks = TRUE, tooltip = 'text') %>%
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




## Shiny App Spray Chart Idea:

# input: player name
# input: date range (or season)
# hover over results: game date, pitch name, hit type, hit result, launch angle, exit velocity, estimated BA, description (inning, teams, score)?
# filter/color by options: hit result, pitch name, hit type
# ouput: plot, title, advanced summary stats 
# custom error message for invalid date range
# fix date so only single season possible
# bonus: change font, other cosmetic touchups
# spraycharts for pitchers (e.g. Shohei Ohtani, Madison Bumgarner, Michael Lorenzen)
# only from 2015 or beyond

