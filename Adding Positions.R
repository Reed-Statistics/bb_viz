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
  select(playerID, P, B, full_name)

positions <- positions %>%
  mutate(player_type = ifelse(P > B, "Pitcher",
                       ifelse(B > P, "Batter", "Both")))

positions <- positions %>%
  select(playerID, player_type) %>%
  rename(key_bbref = playerID)
  


# Improved Player List

players_list <- get_chadwick_lu() %>%
  select(key_mlbam, name_last, name_first, mlb_played_last, key_bbref)

players_list <- players_list %>%
  drop_na(key_mlbam) %>%
  drop_na(key_bbref) %>%
  drop_na(mlb_played_last)

players_list <- players_list %>%
  filter(mlb_played_last > 2007)

players_list <- players_list %>%
  unite("full_name", 3:2, sep = " ") 

players_list <- players_list %>%
  rename(id = key_mlbam)

write_csv(players_list, "/home/leonardr/baseball_viz/players.csv")

players_positions <- left_join(players_list, positions, by = "key_bbref")

players_positions <- players_positions %>%
  distinct(id, .keep_all = TRUE)


# Fixing NAs

players_positions <- players_positions %>%
  drop_na(player_type)

more_players_positions <- read_csv("Player Types - Sheet1 (1).csv")

players_positions <- rbind(players_positions, more_players_positions)

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


# Fixing NAs

batters <- batters %>%
  drop_na(balls_in_play)

more_batters <- read_csv("Player Types - Sheet2.csv")

batters <- rbind(batters, more_batters)


# Filtering Batters

batters <- batters %>%
  filter(balls_in_play > 0)
  

# Write CSV

write_csv(batters, "/home/leonardr/baseball_viz/batters.csv")




