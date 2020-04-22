library(tidyverse)
# Do this once: 
# devtools::install_github("BillPetti/baseballr")
library(baseballr)
correa <- scrape_statcast_savant(start_date = "2019-05-01",
                                 end_date = "2019-05-31",
                                 playerid = 621043,
                                 player_type = "batter")