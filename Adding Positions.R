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


# Fixing NAs and Duplicates

players_positions <- players_positions %>%
  drop_na(player_type) %>%
  filter(full_name != "Carlos Martinez",
         full_name != "Joe Kelly",
         full_name !=  "Luis Garcia",
         full_name != "Adam Eaton",
         full_name != "Carlos Perez",
         full_name != "Charlie Morton",
         full_name != "Chris Hatcher",
         full_name != "Chris Smith",
         full_name != "Chris Young",
         full_name != "Erik Johnson",
         full_name != "Joe Smith",
         full_name != "Jose Bautista",
         full_name != "Jose Fernandez",
         full_name != "Jose Ramirez",
         full_name != "Josh Fields",
         full_name != "Kyle Waldrop",
         full_name != "Nelson Cruz",
         full_name != "Ryan Braun")

more_players_positions <- read_csv("Player Types - Sheet1.csv")

players_positions <- rbind(players_positions, more_players_positions)

players_positions <- players_positions %>%
  filter(player_type != "Both",
         player_type != "Catcher")

pitchers <- players_positions %>%
  filter(player_type == "Pitcher")


# Write CSVs

write_csv(players_positions, "/home/leonardr/baseball_viz/players.csv")

write_csv(pitchers, "/home/leonardr/baseball_viz/pitchers.csv")


# Batter Data

players_list2 <- get_chadwick_lu() %>%
  select(key_bbref, key_mlbam, name_last, name_first, mlb_played_last)

players_list2 <- players_list2 %>%
  drop_na(key_mlbam) %>%
  drop_na(mlb_played_last)

players_list2 <- players_list2 %>%
  filter(mlb_played_last > 2014)

players_list2 <- players_list2 %>%
  unite("full_name", 4:3, sep = " ") 

players_list2 <- players_list2 %>%
  rename(id = key_mlbam)

batters <- players_positions %>%
  filter(mlb_played_last > 2014)

batters <- left_join(batters, players_list2, by = "id")

batters <- batters %>%
  select(id, full_name.x, mlb_played_last.x, player_type, key_bbref) %>%
  rename(full_name = full_name.x,
         mlb_played_last = mlb_played_last.x)

batting <- Batting %>%
  filter(yearID > 2014) %>%
  rename(key_bbref = playerID) %>%
  group_by(key_bbref) %>%
  summarise(balls_in_play = sum(AB) - sum(SO))

batting <- batting %>%
  drop_na(balls_in_play)

batters <- left_join(batters, batting, by = "key_bbref")

batters <- batters %>%
  select(id, full_name, mlb_played_last, player_type, balls_in_play) %>%
  filter(balls_in_play > 0)
  

# Write CSV

write_csv(batters, "/home/leonardr/baseball_viz/batters.csv")




