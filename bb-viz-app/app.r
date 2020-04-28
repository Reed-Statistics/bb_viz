# Load all necessary packages and functions here
library(shiny)
library(shinythemes)
library(tidyverse)
library(baseballr)
library(plotly)
library(viridis)
library(scales)
library(pitchRx)

`%notin%` <- Negate(`%in%`)

# Load all static dataframes here
players <- players

# User interface
ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("An Interactive Baseball Visualization Application"),
                sidebarPanel(),
                mainPanel(
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Spray Chart"),
                    tabPanel("Pitching Chart"),
                    tabPanel("Similarity Search"),
                    tabPanel("Information")
                    )
                  )
                )

# Server function
server <- function(input, output){}

# Creates app
shinyApp(ui = ui, server = server)
