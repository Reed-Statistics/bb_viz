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

## Mutates

# spray_angle = tan((hc_x - 128) / (208 - hc_y))) * 0.75
# hit_location = spray_angle*hit_distance_sc


## Sample Plot

arenado %>%
  filter()



## Shiny App Spray Chart Idea:
  
# input: player name
# input: date range
# color by options: hit result, launch angle, exit velocity
# filter by options: hit result
# ouput: plot, title, summary stats
