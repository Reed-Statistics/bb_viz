#load libraries here
library(shiny)
library(shinythemes)
library(tidyverse)
library(baseballr)
library(plotly)
library(viridis)
library(scales)
library(pitchRx)

#define working directory

head(daily_batter_bref(t1 = "2015-08-01", t2 = "2015-10-03"))
`%notin%` <- Negate(`%in%`)

# Load all static dataframes here
players <- players

# Creates app
shinyApp(ui = ui, server = server)