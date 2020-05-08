## Libraries

library(tidyverse)
library(baseballr)
library(plotly)


## Pull Data

arenado <- scrape_statcast_savant(
  start_date = "2019-03-28",
  end_date = "2019-09-27",
  playerid = 571448,
  player_type = "batter")


## Figure out: missing IBB, wOBA, issues in app (group by year), check other players for more events, improved accuracy

## Summary Table Example

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
  mutate(woba_value = as.numeric(woba_value)) %>%
  mutate(woba_denom = as.numeric(woba_denom)) %>%
  mutate(game_year = as.numeric(game_year)) %>%
  mutate(estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle)) %>%
  mutate(estimated_ba_using_speedangle = na_if(estimated_ba_using_speedangle, "null")) %>%
  group_by(game_year) %>%
  summarise(`Year` = mean(game_year),
            `G` = n_distinct(game_pk),
            `BA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1))/(sum(`AB` == 1)),
            `OBP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1) + sum(`BB` == 1) + sum(`HBP` == 1))/(sum(`PA` == 1)),
            `SLG` = (sum(`1B` == 1) + 2*sum(`2B` == 1) + 3*sum(`3B` == 1) + 4*sum(`HR` == 1))/(sum(`AB` == 1)),
            `OPS` = `OBP` + `SLG`,
            `ISO` = `SLG` - `BA`,
            `wOBA` = mean(woba_value)/mean(woba_denom),
            `BABIP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1))/(sum(`AB` == 1) - sum(`HR` == 1) - sum(`SO` == 1) + sum(`SF` == 1)),
            `xBABIP` = mean(estimated_ba_using_speedangle, na.rm = TRUE),
            `xBA` = `xBABIP`*sum(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play")/(sum(`AB` == 1)),
            `Launch Angle` = mean(launch_angle, na.rm = TRUE),
            `Exit Velocity` = mean(launch_speed, na.rm = TRUE),
            `Hard Hit %` = 100*(sum(launch_speed >= 95, na.rm = TRUE))/(sum(launch_speed >= 95, na.rm = TRUE) + sum(launch_speed < 95, na.rm = TRUE)),
            `Barrel %` = 100*(sum(barrel == 1, na.rm = TRUE))/(sum(barrel == 0, na.rm = TRUE) + sum(barrel == 1, na.rm = TRUE))) %>%
  mutate(`BA` = round(`BA`, 3)) %>%
  mutate(`OBP` = round(`OBP`, 3)) %>%
  mutate(`SLG` = round(`SLG`, 3)) %>%
  mutate(`OPS` = round(`OPS`, 3)) %>%
  mutate(`ISO` = round(`ISO`, 3)) %>%
  mutate(`wOBA` = round(`wOBA`, 3)) %>%
  mutate(`BABIP` = round(`BABIP`, 3)) %>%
  mutate(`xBABIP` = round(`xBABIP`, 3)) %>%
  mutate(`xBA` = round(`xBA`, 3)) %>%
  mutate(`Launch Angle` = round(`Launch Angle`, 1)) %>%
  mutate(`Exit Velocity` = round(`Exit Velocity`, 1)) %>%
  mutate(`Hard Hit %` = round(`Hard Hit %`, 1)) %>%
  mutate(`Barrel %` = round(`Barrel %`, 1))
  
  

  
  
  
