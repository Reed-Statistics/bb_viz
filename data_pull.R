# Do this once
# devtools::install_github("beanumber/statcastr")

library(tidyverse)
library(statcastr)
library(etl)
db <- src_mysql_cnf(dbname = "statcast")
sc <- etl("statcastr", db = db, dir = "~/Data/statcastr")

sc %>%
  etl_extract(year = 2019, month = 4:7) %>%
  etl_transform() %>%
  etl_load()

