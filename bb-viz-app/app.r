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
library(plyr)

#define working directory

head(daily_batter_bref(t1 = "2015-08-01", t2 = "2015-10-03"))
`%notin%` <- Negate(`%in%`)

# Load all static dataframes here
batters <- read_csv("../batters.csv")
pitchers <- read_csv("../pitchers.csv")

# Fonts Properties

font <- list(
  family = "Helvetica Neue",
  size = 14,
  color = 'gray0')

# Scraping Function

scrape_bb_viz <-
  function(start_date,
           end_date,
           playerid,
           player_type) {
    first <- as.numeric(substring(start_date, 1, 4))
    last <- as.numeric(substring(end_date, 1, 4))
    if (first == last) {
      scrape_statcast_savant(
        start_date = start_date,
        end_date = end_date,
        playerid = playerid,
        player_type = player_type
      )
    }
    else {
      dfs <- list(rep(NA, last - first + 1))
      for (i in 0:(last - first)) {
        if (i == 0) {
          dfs[[i + 1]] <- scrape_statcast_savant(
            start_date = start_date,
            end_date = glue("{first + i}-12-31"),
            playerid = playerid,
            player_type = player_type
          )
        }
        else if (i != last - first) {
          dfs[[i + 1]] <-
            scrape_statcast_savant(
              start_date = glue("{first + i}-01-01"),
              end_date = glue("{first + i}-12-31"),
              playerid = playerid,
              player_type = player_type
            )
        }
        else {
          dfs[[i + 1]] <-
            scrape_statcast_savant(
              start_date = glue("{first + i}-01-01"),
              end_date = end_date,
              playerid = playerid,
              player_type = player_type
            )
        }
      }
      return(rbind.fill(dfs))
    }
  }


# User interface
ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "Baseball VisualizeR: An Application for Baseball Visualizations",
                 tabPanel("Spray Chart",
                          sidebarPanel(
                            selectizeInput(inputId = "batter",
                                           choices = batters$full_name,
                                           label = "Select Batter",
                                           selected = "Nolan Arenado"),
                            dateRangeInput(inputId = "hit_dates",
                                           label = "Select Date Range",
                                           min = "2015-04-05",
                                           max = Sys.Date(),
                                           start = "2019-03-28",
                                           end = "2019-9-28"),
                            submitButton("Generate Plot")
                          ),
                          mainPanel(mainPanel(plotlyOutput(outputId = "spray_chart")))),
                 tabPanel("Pitching Chart",
                          sidebarPanel(
                            selectizeInput(inputId = "pitcher",
                                           choices = pitchers$full_name,
                                           label = "Select Pitcher",
                                           selected = "Justin Verlander"),
                            dateRangeInput(inputId = "dates",
                                           label = "Select Date Range",
                                           min = "2008-03-25",
                                           max = Sys.Date(),
                                           start = "2019-03-28",
                                           end = "2019-09-28"),
                            # radioButtons(inputId = "radio",
                            #              label = "Radio buttons",
                            #              choices = list("Pitch Type" = 1, "Speed" = 2), 
                            #              selected = 1),
                            submitButton("Generate Plot")
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
  updateSelectizeInput(session = session, inputId = 'batter')
  
  # Pitching output and data compiling
  pitcher_filter <- reactive({
    pitchers %>%
      filter(full_name == input$pitcher)
  })
  
  pitch_data <- reactive({
    scrape_bb_viz(start_date = input$dates[1],
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
      ggplot(mapping = aes(x = as.numeric(plate_x),
                           y = as.numeric(plate_z),
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
           title = glue("{input$pitcher} Pitches by Pitch Type <br><sub>{input$dates[1]} to {input$dates[2]}<sub>")) +
      xlim(-6,6) +
      theme_void() +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
            plot.title = element_text(hjust = 0.5),
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
      config(displayModeBar = F) %>%
      layout(font = font)
  })
  
  # Batting output and data compiling
  batter_filter <- reactive({
    batters %>%
      filter(full_name == input$batter)
  })
  
  hit_data <- reactive({
    scrape_bb_viz(start_date = input$hit_dates[1],
                           end_date = input$hit_dates[2],
                           playerid = batter_filter()$id,
                           player_type = "batter") %>%
      filter(description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score")) %>%
      mutate(hit_result = ifelse(events == "single", "Single",
                                 ifelse(events == "double", "Double",
                                        ifelse(events == "triple", "Triple",
                                               ifelse(events == "home_run", "Home Run",
                                                      ifelse(events == "sac_fly", "Sacrifice Fly", "Out")))))) %>%
      mutate(hit_type = ifelse(bb_type == "line_drive", "Line Drive",
                               ifelse(bb_type == "fly_ball", "Fly Ball",
                                      ifelse(bb_type == "ground_ball", "Ground Ball", "Pop Fly")))) %>%
      mutate(hit_result = fct_relevel(hit_result, c("Out", "Sacrifice Fly", "Single", "Double", "Triple", "Home Run")))
  })
  
  static_spray_chart <- reactive({
    hit_data() %>%
      ggplot(aes(x = as.numeric(hc_x), y = -as.numeric(hc_y),
                 text = paste('Date: ', game_date, "\n",
                              'Pitch: ', pitch_name, "\n",
                              'Hit Type: ', hit_type, "\n",
                              'Hit Result: ', hit_result, "\n",
                              'Exit Velocity (MPH): ', launch_speed, "\n",
                              'Launch Angle: ', launch_angle, "\n",
                              'Estimated Distance (ft): ', hit_distance_sc, "\n",
                              'Estimated BA: ', estimated_ba_using_speedangle, "\n",
                              'Estimated WOBA: ', estimated_woba_using_speedangle, "\n",
                              sep = ""))) +
      geom_segment(x = 128, xend = 20, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
      geom_segment(x = 128, xend = 236, y = -208, yend = -100, size = 0.7, color = "grey66", lineend = "round") +
      coord_fixed() +
      geom_point(aes(color = hit_result), alpha = 0.6, size = 2) +
      scale_x_continuous(limits = c(25, 225)) +
      scale_y_continuous(limits = c(-225, -25)) +
      labs(color = "Hit Result",
           title = glue("{input$batter} Spray Chart <br><sub>{input$hit_dates[1]} to {input$hit_dates[2]}<sub>")) +
      theme_void() +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
      guides(colour = guide_legend(title.position = "top"))
  })
  
  output$spray_chart <- renderPlotly({
    validate(
      need(
        nrow(hit_data()) != 0,
        "Sorry! The batter that you have selected did hit any balls in play in this time period, according to our data. Please select a different batter or time period."))
    ggplotly(static_spray_chart(), dynamicTicks = TRUE, tooltip = 'text') %>%
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
      config(displayModeBar = F) %>% 
      layout(autosize = F, width = 500, height = 400) %>%
      layout(font = font)
  })
  
  
}

# Creates app
shinyApp(ui = ui, server = server)
