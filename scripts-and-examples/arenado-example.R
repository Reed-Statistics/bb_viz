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


## Static Summary Practice


## Wrangle

arenado <- arenado %>%
  mutate(hit_result = ifelse(events == "single", "Single",
                                    ifelse(events == "double", "Double",
                                           ifelse(events == "triple", "Triple",
                                                  ifelse(events == "home_run", "Home Run",
                                                         ifelse(events == "sac_fly", "Sacrifice Fly", "Out")))))) %>%
  mutate(hit_type = ifelse(bb_type == "line_drive", "Line Drive",
                             ifelse(bb_type == "fly_ball", "Fly Ball",
                                    ifelse(bb_type == "ground_ball", "Ground Ball", "Pop Fly")))) %>%
  mutate(hit_result = fct_relevel(hit_result, c("Out", "Sacrifice Fly", "Single", "Double", "Triple", "Home Run"))) %>%
  select(game_date, player_name, hc_x, hc_y, hit_distance_sc, launch_speed, launch_angle, estimated_ba_using_speedangle, estimated_woba_using_speedangle, hit_result, hit_type)
   
                                              
## Static Summary Practice

arenado_summary <- arenado %>%
  mutate(`1B` = ifelse(events == "single", 1, 0),
         `2B` = ifelse(events == "double", 1, 0),
         `3B` = ifelse(events == "triple", 1, 0),
         `HR` = ifelse(events == "home_run", 1, 0),
         `SF` = ifelse(events == "sac_fly", 1, 0),
         `BB` = ifelse(events == "walk" | events == "hit_by_pitch", 1, 0),
         `HBP` = ifelse(events == "hit_by_pitch", 1, 0),
         `SO` = ifelse(events == "strikeout", 1, 0),
         `AB` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play", 1, 0),
         `PA` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play" | events == "walk" | events == "hit_by_pitch" | events == "sac_fly", 1, 0)) %>%
  filter(`PA` == 1) %>%
  rename(Year = game_year) %>%
  mutate(estimated_ba_using_speedangle = na_if(estimated_ba_using_speedangle, "null")) %>%
  group_by(game_year) %>%
  summarise(`G` = n_distinct(game_pk),
            `BA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1))/(sum(`AB` == 1)),
            `OBP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1) + sum(`BB` == 1) + sum(`HBP` == 1))/(sum(`PA` == 1)),
            `SLG` = (sum(`1B` == 1) + 2*sum(`2B` == 1) + 3*sum(`3B` == 1) + 4*sum(`HR` == 1))/(sum(`AB` == 1)),
            `OPS` = `OBP` + `SLG`,
            `ISO` = `SLG` - `BA`,
            `wOBA` = sum(woba_value)/sum(woba_denom),
            `BABIP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1))/(sum(`AB` == 1) - sum(`HR` == 1) - sum(`SO` == 1) + sum(`SF` == 1)),
            `xBABIP` = mean(estimated_ba_using_speedangle, na.rm = TRUE),
            `xBA` = ,
            `Average LA` = mean(launch_angle, na.rm = TRUE),
            `Average EV` = mean(launch_speed, na.rm = TRUE),
            `Hard Hit-Rate` = (sum(launch_angle >= 95))/(sum(launch_angle >= 95) + sum(launch_angle < 95)),
            `Barrel Rate` = (sum(barrel == 1))/(sum(barrel == 0) + sum(barrel == 1)))
        



## Sample Plot (Spray Chart)

arenado_plot <- arenado %>%
  ggplot(aes(x = hc_x, y = -hc_y)) +
  geom_segment(x = 128, xend = 20, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  geom_segment(x = 128, xend = 236, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
  geom_curve(x = 77, xend = 178, y = -155, yend = -156,
             curvature = -0.65, linetype = "dotted", color = "grey66") +
  coord_fixed() +
  geom_point(aes(color = hit_result), alpha = 0.6, size = 2) +
  scale_x_continuous(limits = c(0, 230)) +
  scale_y_continuous(limits = c(-230, 0)) +
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




