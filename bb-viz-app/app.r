# Load all libraries here
library(shiny)
library(shinythemes)
library(shinydashboard)
library(kableExtra)
library(ggfortify)
library(FNN)
library(tidyverse)
library(baseballr)
library(plotly)
library(viridis)
library(scales)
library(pitchRx)
library(glue)
library(readr)
library(plyr)
library(dplyr)
library(shinycssloaders)
library(DT)

# Load all static dataframes here
batters <- read_csv("../batters.csv")
pitchers <- read_csv("../pitchers.csv")
player_stats <- read_csv("../metrics.csv")
metrics <- read_csv("../metrics.csv")

# Set font properties
font <- list(
  family = "Helvetica Neue",
  size = 14,
  color = 'gray0')

# Load scraping function & "not in" Function
`%notin%` <- Negate(`%in%`)





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


# Define working directory
convert_to_percent <- c("BA", "SLG", "OBP", "OPS", "ISO", "wOBA", "xBA", "xSLG", "xwOBA", "xISO")
player_stats <- read_csv("../metrics.csv")
player_stats <- player_stats %>% 
  mutate(name = paste(Name, Year, sep = ' - '))  %>%
  mutate_at(convert_to_percent, function(d) {d*100})


#predefined variables
relevant_stats <- c("OBP", "xBA" ,"xwOBA", "K %", "BB %", "Sweet Spot %")
stat_choices <- c("BA", "SLG", "OBP", "OPS", "ISO", "wOBA", "xBA", "xSLG", "xwOBA", "xISO", "K %", "BB %", "Launch Angle", "Exit Velocity", "Sweet Spot %", "Barrel %")
table_stats <- c("Name", "Year")
add_web <- function(fig, r, theta, name) {
  fig %>% add_trace(
    r = r,
    theta = theta,
    showlegend = TRUE,
    mode = "markers",
    name = name
  ) 
}
n <- 9

# User interface
ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "Baseball VisualizeR: An Application for Baseball Visualizations",
                 tabPanel("Spray Chart",
                          sidebarPanel(
                            selectizeInput(inputId = "batter",
                                           choices = batters$full_name,
                                           label = "Select Player:",
                                           selected = "Nolan Arenado"),
                            dateRangeInput(inputId = "hit_dates",
                                           label = "Select Date Range:",
                                           min = "2015-04-05",
                                           max = Sys.Date(),
                                           start = "2019-03-28",
                                           end = "2019-09-29",
                                           startview = "year",
                                           autoclose = FALSE),
                            p("Note: Statcast data only collected since 2015"),
                            sliderInput(inputId = "min_launch_angle", 
                                        label = "Select Launch Angle Range:", 
                                        min = -90, 
                                        max = 90, 
                                        value = c(-90, 90)),
                            sliderInput(inputId = "min_exit_velo", 
                                        label = "Select Exit Velocity Range:", 
                                        min = 0, 
                                        max = 125, 
                                        value = c(0, 125)),
                            sliderInput(inputId = "min_distance", 
                                        label = "Select Estimated Distance Range:", 
                                        min = 0, 
                                        max = 525, 
                                        value = c(0, 525)),
                            checkboxGroupInput("hit_result_selection", "Filter by Batted Ball Result:",
                                               choices = list("Out",
                                                              "Sacrifice Fly",
                                                              "Single",
                                                              "Double",
                                                              "Triple",
                                                              "Home Run"),
                                               selected = c("Out",
                                                            "Sacrifice Fly",
                                                            "Single",
                                                            "Double",
                                                            "Triple",
                                                            "Home Run")),
                            checkboxGroupInput("hit_type_selection", "Filter by Batted Ball Type:",
                                               choices = list("Ground Ball",
                                                              "Line Drive",
                                                              "Fly Ball",
                                                              "Pop Fly"),
                                               selected = c("Ground Ball",
                                                            "Line Drive",
                                                            "Fly Ball",
                                                            "Pop Fly")),
                            submitButton("Generate Plot")
                          ),
                          mainPanel(
                            plotlyOutput(outputId = "spray_chart") %>% withSpinner(color="#0dc5c1"),
                            dataTableOutput(outputId = "summary_table"))),
                 tabPanel("Offensive Metrics",
                          sidebarPanel(
                            selectizeInput(inputId = "batterMetrics",
                                           choices = metrics$Name,
                                           label = "Select Player:",
                                           selected = "Jose Abre"),
                            sliderInput(inputId = "season_range", 
                                        label = "Select Season Range:", 
                                        min = 2015, 
                                        max = 2019, 
                                        value = c(2015, 2019),
                                        sep = ""),
                            p("Note: Only includes qualified players for a given season (min. 475 PA)"),
                            submitButton("Generate Data")
                          ),
                          mainPanel(
                          plotOutput(outputId = "metrics_graph") %>% withSpinner(color="#0dc5c1"),
                          dataTableOutput(outputId = "metrics_table"))
                          ),
                 tabPanel("Pitching Chart",
                          sidebarPanel(
                            selectizeInput(inputId = "pitcher",
                                           choices = pitchers$full_name,
                                           label = "Select Player:",
                                           selected = "Felix Hernandez"),
                            dateRangeInput(inputId = "dates",
                                           label = "Select Date Range:",
                                           min = "2008-03-25",
                                           max = Sys.Date(),
                                           start = "2019-03-28",
                                           end = "2019-09-29"),
                            radioButtons(inputId = "radio",
                                         label = "Color By:",
                                         choices = list("Pitch Type" = 1, "Speed" = 2),
                                         selected = 1),
                            sliderInput(inputId = "pitch_speed", 
                                        label = "Select Pitch Speed Range:", 
                                        min = 40, 
                                        max = 110, 
                                        value = c(40, 110)),
                            checkboxGroupInput("pitch_type_selection", "Filter by Pitch Type:",
                                               choices = list("Fastball (4 Seam)",
                                                              "Fastball (2 Seam)",
                                                              "Fastball (Cut)",
                                                              "Sinker",
                                                              "Splitter",
                                                              "Slider",
                                                              "Changeup",
                                                              "Curveball",
                                                              "Knuckle Curve",
                                                              "Knuckleball",
                                                              "Forkball",
                                                              "Eephus",
                                                              "Screwball",
                                                              "Intentional Ball",
                                                              "Pitch out",
                                                              "Unknown"),
                                               selected = c("Fastball (4 Seam)",
                                                            "Fastball (2 Seam)",
                                                            "Fastball (Cut)",
                                                            "Sinker",
                                                            "Splitter",
                                                            "Slider",
                                                            "Changeup",
                                                            "Curveball",
                                                            "Knuckle Curve",
                                                            "Knuckleball",
                                                            "Forkball",
                                                            "Eephus",
                                                            "Screwball",
                                                            "Intentional Ball",
                                                            "Pitch out",
                                                            "Unknown")),
                            submitButton("Generate Plot")
                          ),
                          mainPanel(plotlyOutput(outputId = "pitch_plot") %>% withSpinner(color="#0dc5c1"))),
                 tabPanel("Similarity Search",
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          # pageWithSidebar(
                            # headerPanel('Apply filters'),
                            sidebarPanel(width = 4,
                                         selectInput('player', 'Select Player:', player_stats$name, selected = "Ronald Acuna Jr. - 2019"),
                                         p("Note: Only includes qualified players for a given season (min. 475 PA)"),
                                         checkboxGroupInput(inputId = "selected_stats",
                                                            label = 'Select Metrics to Compare:', choices = stat_choices, 
                                                            selected = relevant_stats),
                                         checkboxGroupInput(inputId = "selected_years",
                                                            label = 'Select Years to Consider:', choices = unique(player_stats$Year), 
                                                            selected = c(2015, 2016, 2017, 2018, 2019)),
                                         submitButton("Generate Data")
                            ),
                            mainPanel(
                              column(8, plotlyOutput("plot1", width = 800, height=700) %>% withSpinner(color="#0dc5c1"),
                                     p("Double click on a player's name in the legend to isolate layer. See table below for ordered comparisons.",
                                       style = "font-size:16px"),
                                     p("Note: For ease of comparison, all metrics have been converted to a percent measurement.",
                                       style = "font-size:14px")
                                     
                              ),
                              dataTableOutput(outputId = "table1")
                            )
                          # )
                 ),
                 tabPanel("User Guide", 
                          p("The metrics used in this app include:",style = "font-size:22px"),
                          hr(),
                          p("BA: Batting Average",style = "font-size:15px"),
                          p("SLG: Slugging Percentage",style = "font-size:15px"),
                          p("OBP: On-Base Percentage",style = "font-size:15px"),
                          p("OPS: On-Base Plus Slugging",style = "font-size:15px"),
                          p("ISO: Isolated Power",style = "font-size:15px"),
                          p("wOBA: Weighted On-Base Average",style = "font-size:15px"),
                          p("xBA: Expected Batting Average (Using Launch Angle and Exit Velocity)",style = "font-size:15px"),
                          p("xSLG: Expected Slugging Percentage (Using Launch Angle and Exit Velocity)",style = "font-size:15px"),
                          p("xISO: Expected Isolated Power (Using Launch Angle and Exit Velocity)",style = "font-size:15px"),
                          p("xwOBA: Expected Weighted On-Base Average (Using Launch Angle and Exit Velocity)",style = "font-size:15px"),
                          p("K %: Strikeout Rate",style = "font-size:15px"),
                          p("BB %: Walk Rate",style = "font-size:15px"),
                          p("Launch Angle: Vertical angle at which the ball leaves a player's bat after being struck",style = "font-size:15px"),
                          p("Exit Velocity: Speed at which the ball leaves a player's bat after being struck",style = "font-size:15px"),
                          p("Estimated Distance: Projected flight path distance of batted ball (Using Launch Angle and Exit Velocity)",style = "font-size:15px"),
                          p("Hard Hit %: Proportion of batted balls with an exit velocity equal or greater to 95 MPH",style = "font-size:15px"),
                          p("Sweet Spot %: Proportion of batted balls with a launch angle between 8 and 32 degrees",style = "font-size:15px"),
                          p("Barrel %: Proportion of batted-ball events whose comparable hit types (in terms of exit velocity and launch angle) have led to a minimum .500 batting average and 1.500 slugging percentage",style = "font-size:15px")
                          ),
                 tabPanel("Developers & Sources",
                          tags$p("This app was created by",
                                 tags$a("Riley Leonard", href = "https://www.linkedin.com/in/riley-leonard-9653791a6/", taget = "_blank"), 
                                 ",  ",
                                 tags$a("Jonathan Li", href = "https://github.com/jonathanmli", taget = "_blank"),
                                 ", and",
                                 tags$a("Grayson White", href = "https://www.github.com/graysonwhite", taget = "_blank"),
                                 "as a final project for Math 241: Data Science at Reed College in Spring 2020.
                                 The goal of this app is to allow baseball fans to explore and visualize many
                                 of the advanced metrics that have been recorded for Major League Baseball since 2008."),
                          tags$p(
                            "The data used for this app was primarily pulled from",
                            tags$a("Statcast,", href="https://baseballsavant.mlb.com/statcast_search", target="_blank"),
                            "and tools were used from baseball packages such as",
                            tags$code("Lahman"),
                            ", ",
                            tags$code("baseballr"),
                            ", and",
                            tags$code("pitchRx"),
                            ".")
                 )
)

# Server function
server <- function(input, output, session){
  updateSelectizeInput(session = session, inputId = 'pitcher')
  updateSelectizeInput(session = session, inputId = 'batter')
  updateSelectizeInput(session = session, inputId = 'batterMetrics', selected = "Jose Abreu")
  
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
      ) %>%
      dplyr::filter(pitch_type %in% input$pitch_type_selection) %>%
      dplyr::filter(release_speed >= input$pitch_speed[1],
                    release_speed <= input$pitch_speed[2])
  })
  
  static_plot <- reactive({
    if(input$radio == 1) {
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
    }
    else {
      pitch_data() %>%
        filter(pitch_type %notin% c("IN", "null")) %>%
        ggplot(mapping = aes(x = as.numeric(plate_x),
                             y = as.numeric(plate_z),
                             color = release_speed,
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
        scale_color_viridis_c() +
        labs(color = "Release Speed",
             title = glue("{input$pitcher} Pitches by Release Speed <br><sub>{input$dates[1]} to {input$dates[2]}<sub>")) +
        xlim(-6,6) +
        theme_void() +
        theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
              plot.title = element_text(hjust = 0.5),
              legend.position = "bottom") 
    }
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
      layout(autosize = F, width = 600, height = 600) %>%
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
      mutate(hit_result = ifelse(events == "single", "Single",
                                 ifelse(events == "double", "Double",
                                        ifelse(events == "triple", "Triple",
                                               ifelse(events == "home_run", "Home Run",
                                                      ifelse(events == "sac_fly", "Sacrifice Fly", "Out")))))) %>%
      mutate(hit_type = ifelse(bb_type == "line_drive", "Line Drive",
                               ifelse(bb_type == "fly_ball", "Fly Ball",
                                      ifelse(bb_type == "ground_ball", "Ground Ball", "Pop Fly")))) %>%
      mutate(hit_result = fct_relevel(hit_result, c("Out", "Sacrifice Fly", "Single", "Double", "Triple", "Home Run"))) %>%
      filter(hit_result %in% input$hit_result_selection) %>%
      filter(hit_type %in% input$hit_type_selection) %>%
      filter(launch_angle >= input$min_launch_angle[1],
             launch_angle <= input$min_launch_angle[2]) %>%
      filter(launch_speed >= input$min_exit_velo[1],
             launch_speed <= input$min_exit_velo[2]) %>%
      filter(hit_distance_sc >= input$min_distance[1],
             hit_distance_sc <= input$min_distance[2]) 
  })
  
  static_spray_chart <- reactive({
    hit_data() %>%
      ggplot(aes(x = as.numeric(hc_x), y = -as.numeric(hc_y),
                 text = paste('Date: ', game_date, "\n",
                              'Home Team: ', home_team, "\n",
                              'Away Team: ', away_team, "\n",
                              'Pitch: ', pitch_name, "\n",
                              'Batted Ball Type: ', hit_type, "\n",
                              'Batted Ball Result: ', hit_result, "\n",
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
      scale_color_manual(values = c(`Out` = "#F8766D",
                                    `Sacrifice Fly` = "#C59500",
                                    `Single` = "#00BA42",
                                    `Double` = "#00B4E4",
                                    `Triple` = "#AC88FF",
                                    `Home Run` = "#F066EA")) +
      scale_x_continuous(limits = c(0, 230)) +
      scale_y_continuous(limits = c(-230, 0)) +
      labs(color = "Hit Result",
           title = glue("{input$batter} Spray Chart <br><sub>{input$hit_dates[1]} to {input$hit_dates[2]}<sub>")) +
      theme_void() +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
            plot.title = element_text(hjust = 0.5)) +
      guides(colour = guide_legend(title.position = "top"))
  })
  
  output$spray_chart <- renderPlotly({
    validate(
      need(
        nrow(hit_data()) != 0,
        "Sorry! The batter that you have selected did not hit any balls in play of the given specifications in this time period, according to our data. Please adjust your filters or select a different batter or time period."))
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
      layout(autosize = F, width = 575, height = 425) %>%
      layout(font = font)
  })
  
  output$summary_table <- renderDataTable({
    datatable(hit_data() %>%
                mutate(`1B` = ifelse(events == "single", 1, 0),
                       `2B` = ifelse(events == "double", 1, 0),
                       `3B` = ifelse(events == "triple", 1, 0),
                       `HR` = ifelse(events == "home_run", 1, 0),
                       `SF` = ifelse(events == "sac_fly", 1, 0),
                       `BB` = ifelse(events == "walk" | events == "hit_by_pitch", 1, 0),
                       `HBP` = ifelse(events == "hit_by_pitch", 1, 0),
                       `SO` = ifelse(events == "strikeout", 1, 0),
                       `AB` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play", 1, 0),
                       `PA` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play" | events == "walk" | events == "hit_by_pitch" | events == "sac_fly", 1, 0)) %>%
                filter(`PA` == 1) %>%
                mutate(woba_value = as.numeric(woba_value)) %>%
                mutate(Year = as.factor(game_year)) %>%
                mutate(woba_denom = as.numeric(woba_denom)) %>%
                mutate(estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle)) %>%
                mutate(estimated_ba_using_speedangle = na_if(estimated_ba_using_speedangle, "null")) %>%
                dplyr::group_by(Year) %>%
                dplyr::summarise(`G` = n_distinct(game_pk),
                                 `Average Launch Angle` = mean(launch_angle, na.rm = TRUE),
                                 `Average Exit Velocity` = mean(launch_speed, na.rm = TRUE),
                                 `Average Distance` = mean(hit_distance_sc, na.rm = TRUE),
                                 `Max Exit Velocity` = max(launch_speed, na.rm = TRUE),
                                 `Max Distance` = max(hit_distance_sc, na.rm = TRUE),
                                 `Hard Hit %` = 100*(sum(launch_speed >= 95, na.rm = TRUE))/(sum(launch_speed >= 95, na.rm = TRUE) + sum(launch_speed < 95, na.rm = TRUE)),
                                 `Sweet Spot %` = 100*(sum(launch_angle > 8 & launch_angle < 32, na.rm = TRUE))/(sum(launch_angle > -91, na.rm = TRUE)),
                                 `Barrel %` = 100*(sum(barrel == 1, na.rm = TRUE))/(sum(barrel == 0, na.rm = TRUE) + sum(barrel == 1, na.rm = TRUE))) %>%
                mutate(`Average Launch Angle` = format(round(`Average Launch Angle`, 1), nsmall = 1)) %>%
                mutate(`Average Exit Velocity` = format(round(`Average Exit Velocity`, 1), nsmall = 1)) %>%
                mutate(`Average Distance` = format(round(`Average Distance`, 1), nsmall = 1)) %>%
                mutate(`Hard Hit %` = format(round(`Hard Hit %`, 1), nsmall = 1)) %>%
                mutate(`Sweet Spot %` = format(round(`Sweet Spot %`, 1), nsmall = 1)) %>%
                mutate(`Barrel %` = format(round(`Barrel %`, 1), nsmall = 1)),
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = FALSE,
                             ordering = FALSE))
  })
  
  # Metrics output and data compiling
  
  metrics_filter <- reactive({
    metrics %>%
      filter(Name == input$batterMetrics)
  })
  
  output$metrics_graph <- renderPlot({
    metrics_filter() %>%
      filter(Year >= input$season_range[1],
             Year <= input$season_range[2]) %>%
      select(Year, BA, xBA, wOBA, xwOBA) %>%
      tidyr::pivot_longer(cols = c(BA, xBA, wOBA, xwOBA), names_to = "Metric", values_to = "Value") %>%
      mutate(Metric = fct_relevel(Metric, "BA", "xBA", "wOBA", "xwOBA")) %>%
      ggplot(mapping = aes(x = Metric, y = Value)) +
      geom_col(aes(fill = Metric)) +
      scale_fill_manual(values = c(`BA` = "#00B4E4",
                                   `xBA` = "#AC88FF",
                                   `wOBA` = "#00B4E4",
                                   `xwOBA` = "#AC88FF")) +
      labs(title = glue("{input$batterMetrics} Offensive Metrics"),
           x = "Metric", 
           y = "Value") +
      facet_wrap(~Year, nrow = 1) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$metrics_table <- renderDataTable({
    datatable(metrics_filter() %>%
                filter(Year >= input$season_range[1],
                       Year <= input$season_range[2]) %>%
                select(Year, BA, OBP, SLG, OPS, ISO, wOBA, xBA, xISO, xwOBA, `K %`, `BB %`) %>%
                mutate(`BA` = format(round(`BA`, 3), nsmall = 3)) %>%
                mutate(`OBP` = format(round(`OBP`, 3), nsmall = 3)) %>%
                mutate(`SLG` = format(round(`SLG`, 3), nsmall = 3)) %>%
                mutate(`OPS` = format(round(`OPS`, 3), nsmall = 3)) %>%
                mutate(`ISO` = format(round(`ISO`, 3), nsmall = 3)) %>%
                mutate(`xBA` = format(round(`xBA`, 3), nsmall = 3)) %>%
                mutate(`wOBA` = format(round(`wOBA`, 3), nsmall = 3)) %>%
                mutate(`xISO` = format(round(`xISO`, 3), nsmall = 3)) %>%
                mutate(`xwOBA` = format(round(`xwOBA`, 3), nsmall = 3)),
    options = list(paging = FALSE,
                   searching = FALSE,
                   orderClasses = TRUE,
                   ordering = TRUE))
  })
  
  
  #remove input player from data
  selectedData1 <- reactive({
    player_stats%>%
      filter(player_stats$name != input$player)
  })
  
  
  #conduct filtering
  selectedData2 <- reactive({
    selectedData1() %>%
      # select(c(stat_choices, name)) 
      filter(Year == input$selected_years)
    #   filter(selectedData1()$position %in% input$position,
    #          selectedData1()$foot %in% input$foot) %>%
    #   filter(overall >= input$overall[1]) %>%
    #   filter(overall <= input$overall[2]) %>%
    #   filter(height >= input$height[1])  %>%
    #   filter(height <= input$height[2])
  })
  
  #select target player data
  selectedData3 <- reactive({
    player_stats%>%
      filter(player_stats$name == input$player)
    
  })
  
  #bind the two tables together into master table
  selectedData4 <- reactive({
    rbind(selectedData3(),selectedData2())
    
  })
  #select the numericss that we are clustering on
  selectedData5 <- reactive({
    selectedData4() %>%
      select(input$selected_stats)
  })
  #conduct clustering
  selectedData6 <- reactive({
    as.numeric(knnx.index(selectedData5(), selectedData5()[1, , drop=FALSE], k=n+1))
  })
  #select chosen players
  selectedData7 <- reactive({
    selectedData4()[selectedData6(),]
  })
  
  #select relvant stats
  selectedData8 <- reactive({
    selectedData7() %>%
      select(input$selected_stats)
  })
  # 
  # 
  # # Combine the selected variables into a new data frame
  output$plot1 <- renderPlotly({
    
    validate(
      need(dim(selectedData2())[1]>=n, "Sorry, no ten similar players were found.
           Please change the input filters."
      )
    )
    
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) 
    for (i in 1:(n+1)) {
      fig <- add_web(fig, 
                     as.matrix(selectedData8()[i,]), 
                     input$selected_stats, 
                     as.character(selectedData7()$name[i]))
    }
    fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),
        showlegend=TRUE
      )
    fig
    
  })
  output$table1 <- renderDataTable({
    datatable(selectedData7() %>%
                select(c(table_stats,input$selected_stats)),
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = TRUE,
                             ordering = TRUE))
  })
}

# Creates app
shinyApp(ui = ui, server = server)
