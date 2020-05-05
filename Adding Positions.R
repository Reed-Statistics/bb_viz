# Libraries

library(tidyverse)
library(Lahman)
library(baseballr)

players <- players

fielding <- Fielding

names <- People %>%
  select(playerID, nameFirst, nameLast) %>%
  unite("full_name", 2:3, sep = " ")

positions <- Fielding %>%
  group_by(playerID) %>%
  summarise(P = sum(POS == "P"),
            B = sum(POS != "P"))

positions <- left_join(positions, names, by = "playerID") %>%
  select(P, B, full_name.x) %>%
  rename(full_name = full_name.x)

positions <- positions %>%
  mutate(player_type = ifelse(P > B, "Pitcher",
                       ifelse(B > P, "Batter", "Both")))

positions <- positions %>%
  select(full_name, player_type)

write_csv(positions, "/home/leonardr/baseball_viz/positions.csv")
  
  
  
  
  