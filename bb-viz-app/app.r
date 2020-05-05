#load libraries here
library(shiny)
library(shinythemes)
library(tidyverse)
library(baseballr)
library(plotly)
library(viridis)
library(scales)
library(pitchRx)
library(glue)
library(readr)

#define working directory

head(daily_batter_bref(t1 = "2015-08-01", t2 = "2015-10-03"))
`%notin%` <- Negate(`%in%`)

# Load all static dataframes here
players <- players

positions <- read_csv("../positions.csv")
pitchers <- positions %>%
  filter(player_type == "Pitcher") %>%
  inner_join(players) 

# User interface
ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "Baseball VisualizeR: An Application for Baseball Visualizations",
                 tabPanel("Spray Chart",
                          sidebarPanel(),
                          mainPanel()),
                 tabPanel("Pitching Chart",
                          sidebarPanel(
                            selectizeInput(inputId = "pitcher",
                                           choices = pitchers$full_name,
                                           label = "Select pitcher",
                                           selected = "Justin Verlander"),
                            p("Do not select date ranges outside of the same calendar year."),
                            dateRangeInput(inputId = "dates",
                                           label = "Date Range",
                                           min = "2008-03-25",
                                           max = Sys.Date(),
                                           start = "2011-05-01",
                                           end = "2011-05-31"),
                            radioButtons(inputId = "radio",
                                         label = "Radio buttons",
                                         choices = list("Pitch Type" = 1, "Speed" = 2), 
                                         selected = 1),
                            submitButton("Update Plot")
                          ),
                          mainPanel(plotlyOutput(outputId = "pitch_plot"))),
                 tabPanel("Similarity Search",
                          sidebarPanel(),
                          mainPanel()),
                 tabPanel("Information",
                          mainPanel(
                            p("This app was created by Riley Leonard, Jonathan Li, and Grayson White
                              as a final project for Math 241: Data Science at Reed College in Spring 2020.
                              The goal of this app is to allow the user to explore and visualize many
                              of the advanced metrics that have been recorded for Major League Baseball since 2008.")
                          ))
)

# Server function
server <- function(input, output, session){
  updateSelectizeInput(session = session, inputId = 'pitcher')
  
  # Pitching output and data compiling
  pitcher_filter <- reactive({
    pitchers %>%
      filter(full_name == input$pitcher)
  })
  
  pitch_data <- reactive({
    scrape_statcast_savant(start_date = input$dates[1],
                           end_date = input$dates[2],
                           playerid = pitcher_filter()$id,
                           player_type = "pitcher") %>%
      mutate(
        description = case_when(
          description %in% c("called_strike",
                             "swinging_strike_blocked") ~ "Called Strike",
          description == "swinging_strike" ~ "Swinging Strike",
          description == "blocked_ball" ~ "Blocked Ball",
          description %in% c("ball",
                             "intent_ball") ~ "Ball",
          description %in% c(
            "hit_into_play_no_out",
            "hit_into_play_score",
            "hit_into_play"
          ) ~ "Hit into Play",
          description == "pitchout" ~ "Pitch Out",
          description %in% c("foul_tip",
                             "foul") ~ "Foul",
          description %in% c("missed_bunt",
                             "foul_bunt") ~ "Bunt Attempt",
          description == "hit_by_pitch" ~ "Hit By Pitch"
        ),
        pitch_type = case_when(
          pitch_type == "FF" ~ "Fastball (4 Seam)",
          pitch_type == "FT" ~ "Fastball (2 Seam)",
          pitch_type == "FC" ~ "Fastball (Cut)",
          pitch_type == "SI" ~ "Sinker",
          pitch_type == "FS" ~ "Splitter",
          pitch_type == "SL" ~ "Slider",
          pitch_type == "CH" ~ "Changeup",
          pitch_type == "CU" ~ "Curveball",
          pitch_type == "KC" ~ "Knuckle Curve",
          pitch_type == "KN" ~ "Knuckleball",
          pitch_type == "FO" ~ "Forkball",
          pitch_type == "EP" ~ "Eephus",
          pitch_type == "SC" ~ "Screwball",
          pitch_type == "IN" ~ "Intentional Ball",
          pitch_type == "PO" ~ "Pitch out",
          pitch_type == "UN" ~ "Unknown"
        )
      )
  })
  
  static_plot <- reactive({
    pitch_data() %>%
      filter(pitch_type %notin% c("IN", "null")) %>%
      ggplot(mapping = aes(x = plate_x,
                           y = plate_z,
                           color = pitch_type,
                           text = paste('Date: ', game_date, "\n",
                                        'Pitch: ', pitch_type, "\n",
                                        'Release Speed: ', release_speed, "\n",
                                        'Result: ', description, "\n",
                                        sep = "")
      )
      ) +
      coord_fixed() +
      geom_point(alpha = 0.5) +
      geom_segment(x = -0.85, xend = 0.85, y = 3.5, yend = 3.5, size = 0.7, color = "black", lineend = "round") +
      geom_segment(x = -0.85, xend = 0.85, y = 1.5, yend = 1.5, size = 0.7, color = "black", lineend = "round") +
      geom_segment(x = -0.85, xend = -0.85, y = 1.5, yend = 3.5, size = 0.7, color = "black", lineend = "round") +
      geom_segment(x = 0.85, xend = 0.85, y = 1.5, yend = 3.5, size = 0.7, color = "black", lineend = "round") +
      scale_color_viridis_d() +
      labs(color = "Pitch Type",
           title = glue("{input$pitcher} Pitches by Pitch Type"),
           subtitle = "2014 MLB Season") +
      xlim(-6,6) +
      theme_void() +
      theme(plot.background = element_rect(fill = "grey96"),
            plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5, face = "italic"),
            legend.position = "bottom") +
      guides(colour = guide_legend(title.position = "top"))
  })
  
  output$pitch_plot <- renderPlotly({
    validate(
      need(
      nrow(pitch_data()) != 0,
      "Sorry! The pitcher that you have selected did not throw any pitches in this time period, according to our data. Please select a different pitcher or time period."))
    ggplotly(static_plot(), dynamicTicks = TRUE, tooltip = 'text') %>%
      layout(xaxis = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE), 
        yaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE)) %>% 
      config(displayModeBar = F)
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)
