## Libraries

library(baseballr)
library(plotly)
library(dplyr)
library(tidyverse)
library(reshape2)

## Pull Data

arenado <- scrape_statcast_savant(
  start_date = "2019-03-28",
  end_date = "2019-09-27",
  playerid = 571448,
  player_type = "batter")


## Figure out: aggregate plot metric v. value (with custom title), join with Lahman (two seperate data tables), color blue, missing foul balls

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
  mutate(Year = as.factor(game_year)) %>%
  mutate(estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle)) %>%
  mutate(estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle)) %>%
  mutate(estimated_ba_using_speedangle = na_if(estimated_ba_using_speedangle, "null")) %>%
  mutate(estimated_woba_using_speedangle = na_if(estimated_woba_using_speedangle, "null")) %>%
  group_by(Year) %>%
  summarise(`G` = n_distinct(game_pk),
            `BA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1))/(sum(`AB` == 1)),
            `OBP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1) + sum(`BB` == 1) + sum(`HBP` == 1))/(sum(`PA` == 1)),
            `SLG` = (sum(`1B` == 1) + 2*sum(`2B` == 1) + 3*sum(`3B` == 1) + 4*sum(`HR` == 1))/(sum(`AB` == 1)),
            `OPS` = `OBP` + `SLG`,
            `ISO` = `SLG` - `BA`,
            `BABIP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1))/(sum(`AB` == 1) - sum(`HR` == 1) - sum(`SO` == 1) + sum(`SF` == 1)),
            `xBABIP` = (mean(estimated_ba_using_speedangle[events != "home_run"], na.rm = TRUE)),
            `xBA` = mean(estimated_ba_using_speedangle, na.rm = TRUE)*sum(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play")/(sum(`AB` == 1)),
            `Barrel %` = 100*(sum(barrel == 1, na.rm = TRUE))/(sum(barrel == 0, na.rm = TRUE) + sum(barrel == 1, na.rm = TRUE))) %>%
  mutate(`BA` = format(round(`BA`, 3), nsmall = 3)) %>%
  mutate(`OBP` = format(round(`OBP`, 3), nsmall = 3)) %>%
  mutate(`SLG` = format(round(`SLG`, 3), nsmall = 3)) %>%
  mutate(`OPS` = format(round(`OPS`, 3), nsmall = 3)) %>%
  mutate(`ISO` = format(round(`ISO`, 3), nsmall = 3)) %>%
  mutate(`BABIP` = format(round(`BABIP`, 3), nsmall = 3)) %>%
  mutate(`xBABIP` = format(round(`xBABIP`, 3), nsmall = 3)) %>%
  mutate(`xBA` = format(round(`xBA`, 3), nsmall = 3)) %>%
  mutate(`Barrel %` = format(round(`Barrel %`, 1), nsmall = 1)) %>%
  ggplot(mapping = aes(x = `Year`, y = `xBA`)) +
  geom_col(size = 1.2) +
  labs(x = "Year", 
       y = "Expected Batting Average (xBA)") +
theme_minimal()
  

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
            `xBABIP` = mean(estimated_ba_using_speedangle[(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 0)], na.rm = TRUE),
            `xBA` = mean(estimated_ba_using_speedangle, na.rm = TRUE)*sum(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play")/(sum(`AB` == 1)),
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

## Summary Plot

arenado_plot_summary <- arenado %>%
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
  mutate(Year = as.factor(game_year)) %>%
  mutate(estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle)) %>%
  mutate(estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle)) %>%
  mutate(estimated_ba_using_speedangle = na_if(estimated_ba_using_speedangle, "null")) %>%
  mutate(estimated_woba_using_speedangle = na_if(estimated_woba_using_speedangle, "null")) %>%
  group_by(Year) %>%
  summarise(`BA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1))/(sum(`AB` == 1)),
            `OBP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1) + sum(`BB` == 1) + sum(`HBP` == 1))/(sum(`PA` == 1)),
            `SLG` = (sum(`1B` == 1) + 2*sum(`2B` == 1) + 3*sum(`3B` == 1) + 4*sum(`HR` == 1))/(sum(`AB` == 1)),
            `ISO` = `SLG` - `BA`,
            `BABIP` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1))/(sum(`AB` == 1) - sum(`HR` == 1) - sum(`SO` == 1) + sum(`SF` == 1)),
            `xBABIP` = (mean(estimated_ba_using_speedangle[events != "home_run"], na.rm = TRUE)),
            `xBA` = mean(estimated_ba_using_speedangle, na.rm = TRUE)*sum(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play")/(sum(`AB` == 1))) %>%
  mutate(`BA` = format(round(`BA`, 3), nsmall = 3)) %>%
  mutate(`OBP` = format(round(`OBP`, 3), nsmall = 3)) %>%
  mutate(`SLG` = format(round(`SLG`, 3), nsmall = 3)) %>%
  mutate(`ISO` = format(round(`ISO`, 3), nsmall = 3)) %>%
  mutate(`BABIP` = format(round(`BABIP`, 3), nsmall = 3)) %>%
  mutate(`xBABIP` = format(round(`xBABIP`, 3), nsmall = 3)) %>%
  mutate(`xBA` = format(round(`xBA`, 3), nsmall = 3)) %>%
  select(BA, ISO, BABIP, xBABIP, xBA) %>%
  tidyr::pivot_longer(cols = c(BA, ISO, BABIP, xBABIP, xBA), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = fct_relevel(Metric, "BA", "xBA", "BABIP", "xBABIP", "ISO")) %>%
  ggplot(mapping = aes(x = Metric, y = Value)) +
  geom_col(color = "#00B4E4", fill = "#00B4E4") +
  labs(title = glue("{input$batterMetrics} Offensive Metrics: {input$metrics_dates[1]} to {input$metrics_dates[2]}"),
       x = "Metric", 
       y = "Value") +
  theme_minimal()


## Stats

stats <- read_csv("stats.csv")

stats <- stats %>%
  arrange(year) %>%
  arrange(last_name) %>%
  unite("full_name", 2:1, sep = " ") %>%
  dplyr::rename(Year = year,
                AB = b_ab,
                PA = b_total_pa,
                BA = batting_avg,
                SLG = slg_percent,
                OBP = on_base_percent,
                OPS = on_base_plus_slg,
                wOBA = woba,
                xBA = xba,
                xSLG = xslg,
                xwOBA = xwoba,
                xISO = xiso,
                `K %` = b_k_percent,
                `BB %` = b_bb_percent,
                `Launch Angle` = launch_angle_avg,
                `Exit Velocity` = exit_velocity_avg,
                `Sweet Spot %` = sweet_spot_percent,
                `Barrel %` = barrel_batted_rate) %>%
  mutate(ISO = SLG - BA) %>%
  select(full_name, Year, AB, PA, BA, SLG, OBP, OPS, ISO, wOBA, xBA, xSLG, xwOBA, xISO, `K %`, `BB %`, `Launch Angle`, `Exit Velocity`, `Sweet Spot %`, `Barrel %`)


write_csv(stats, "/home/leonardr/baseball_viz/metrics.csv")

metrics <- read_csv("/home/leonardr/baseball_viz/metrics.csv")

convert_to_percent <- c("BA", "SLG", "OBP", "OPS", "ISO", "wOBA", "xBA", "xSLG", "xwOBA", "xISO")
relevant_stats <- c("OBP", "xBA" ,"xwOBA", "K %", "BB %", "Sweet Spot %")
stat_choices <- c("BA", "SLG", "OBP", "OPS", "ISO", "wOBA", "xBA", "xSLG", "xwOBA", "xISO", "K %", "BB %", "Launch Angle", "Exit Velocity", "Sweet Spot %", "Barrel %")

mookie <- metrics %>%
  filter(full_name == "Mookie Betts") %>%
  select(Year, BA, xBA, wOBA, xwOBA) %>%
  tidyr::pivot_longer(cols = c(BA, xBA, wOBA, xwOBA), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = fct_relevel(Metric, "BA", "xBA", "wOBA", "xwOBA")) %>%
  ggplot(mapping = aes(x = Metric, y = Value)) +
  geom_col(color = "#00B4E4", fill = "#00B4E4") +
  labs(title = glue("Mookie"),
       x = "Metric", 
       y = "Value") +
  facet_wrap(~Year, nrow = 1) +
  theme_minimal()
  
  

