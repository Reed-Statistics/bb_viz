library(baseballr)
library(tidyverse)
library(glue)

scrape_bb_viz <- function(start_date, end_date, playerid, player_type) {
  first <- as.numeric(substring(start_date, 1, 4))
  last <- as.numeric(substring(end_date, 1, 4))
  if(first == last) {
    scrape_statcast_savant(start_date = start_date,
                           end_date = end_date,
                           playerid = playerid,
                           player_type = player_type) 
  }
  else {
    dfs <- list(rep(NA, last-first+1))
    for(i in 0:(last-first)) {
      if(i == 0) {
        dfs[[i+1]] <- scrape_statcast_savant(start_date = start_date,
                               end_date = glue("{first + i}-12-31"),
                               playerid = playerid,
                               player_type = player_type)
      }
      else if(i != last-first) {
        dfs[[i+1]] <- scrape_statcast_savant(start_date = glue("{first + i}-01-01"),
                               end_date = glue("{first + i}-12-31"),
                               playerid = playerid,
                               player_type = player_type)
      }
      else {
        dfs[[i+1]] <- scrape_statcast_savant(start_date = glue("{first + i}-01-01"),
                               end_date = end_date,
                               playerid = playerid,
                               player_type = player_type)
      }
    }
  }
  return(bind_rows(dfs))
}

felix <- scrape_bb_viz(start_date = "2013-03-20",
                                end_date = "2016-11-10",
                                playerid = 433587,
                                player_type = "pitcher")