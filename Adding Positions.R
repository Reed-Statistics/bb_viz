# Libraries

library(tidyverse)
library(Lahman)
library(baseballr)


# Adding Positions

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
  
positions <- read_csv("/home/leonardr/baseball_viz/positions.csv")


# Improved Player List

players_list <- get_chadwick_lu() %>%
  select(key_mlbam, name_last, name_first, mlb_played_last)

players_list <- players_list %>%
  drop_na(key_mlbam) %>%
  drop_na(mlb_played_last)

players_list <- players_list %>%
  filter(mlb_played_last > 2007)

players_list <- players_list %>%
  unite("full_name", 3:2, sep = " ") 

players_list <- players_list %>%
  rename(id = key_mlbam)

write_csv(players_list, "/home/leonardr/baseball_viz/players.csv")

players_positions <- left_join(players_list, positions, by = "full_name")

players_positions <- players_positions %>%
  distinct(id, .keep_all = TRUE)

write_csv(players_positions, "/home/leonardr/baseball_viz/players.csv")



