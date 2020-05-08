## Libraries

library(baseballr)
library(plotly)
library(dplyr)
library(tidyverse)


## Pull Data

arenado <- scrape_statcast_savant(
  start_date = "2019-03-28",
  end_date = "2019-09-27",
  playerid = 571448,
  player_type = "batter")


## Figure out: missing IBB, wOBA, join with Lahman (two seperate data tables), glossary?

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
         `AB` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play", 1, 0),
         `PA` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play" | events == "walk" | events == "hit_by_pitch" | events == "sac_fly", 1, 0)) %>%
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
  

## Pull Data

scrape_bb_viz <-
  function(start_date,
           end_date,
           playerid,
           player_type) {
    first <- as.numeric(substring(start_date, 1, 4))
    last <- as.numeric(substring(end_date, 1, 4))
    if (first == last) {
      scrape_statcast_savant(
        start_date = start_date,
        end_date = end_date,
        playerid = playerid,
        player_type = player_type
      )
    }
    else {
      dfs <- list(rep(NA, last - first + 1))
      for (i in 0:(last - first)) {
        if (i == 0) {
          dfs[[i + 1]] <- scrape_statcast_savant(
            start_date = start_date,
            end_date = glue("{first + i}-12-31"),
            playerid = playerid,
            player_type = player_type
          )
        }
        else if (i != last - first) {
          dfs[[i + 1]] <-
            scrape_statcast_savant(
              start_date = glue("{first + i}-01-01"),
              end_date = glue("{first + i}-12-31"),
              playerid = playerid,
              player_type = player_type
            )
        }
        else {
          dfs[[i + 1]] <-
            scrape_statcast_savant(
              start_date = glue("{first + i}-01-01"),
              end_date = end_date,
              playerid = playerid,
              player_type = player_type
            )
        }
      }
      return(rbind.fill(dfs))
    }
  }

arenado_career <- scrape_bb_viz(start_date = "2015-04-05",
              end_date = "2019-09-27",
              playerid = 571448,
              player_type = "batter")

## Career Summary

arenado_career %>%
  mutate(`1B` = ifelse(events == "single", 1, 0),
         `2B` = ifelse(events == "double", 1, 0),
         `3B` = ifelse(events == "triple", 1, 0),
         `HR` = ifelse(events == "home_run", 1, 0),
         `SF` = ifelse(events == "sac_fly", 1, 0),
         `BB` = ifelse(events == "walk" | events == "hit_by_pitch", 1, 0),
         `HBP` = ifelse(events == "hit_by_pitch", 1, 0),
         `SO` = ifelse(events == "strikeout", 1, 0),
         `AB` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play", 1, 0),
         `PA` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play" | events == "walk" | events == "hit_by_pitch" | events == "sac_fly", 1, 0)) %>%
  filter(`PA` == 1) %>%
  mutate(woba_value = as.numeric(woba_value)) %>%
  mutate(Year = as.factor(game_year)) %>%
  mutate(woba_denom = as.numeric(woba_denom)) %>%
  mutate(estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle)) %>%
  mutate(estimated_ba_using_speedangle = na_if(estimated_ba_using_speedangle, "null")) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(`G` = n_distinct(game_pk),
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
  
  

