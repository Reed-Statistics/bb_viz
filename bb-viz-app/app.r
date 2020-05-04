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

#define working directory

head(daily_batter_bref(t1 = "2015-08-01", t2 = "2015-10-03"))
`%notin%` <- Negate(`%in%`)

# Load all static dataframes here
players <- players

# User interface
ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "An Interactive Baseball Visualization Application",
                 tabPanel("Spray Chart",
                          sidebarPanel(),
                          mainPanel()),
                 tabPanel("Pitching Chart",
                          sidebarPanel(
                            p("Note that the drop down
                              currently includes all players, so please select a player
                              that you know is a pitcher."),
                            selectizeInput(inputId = "pitcher",
                                           choices = players$full_name,
                                           label = "Select pitcher",
                                           selected = NULL),
                            dateRangeInput(inputId = "dates",
                                           label = "Date Range",
                                           min = "2008-03-25",
                                           max = Sys.Date())
                          ),
                          mainPanel(plotlyOutput(outputId = "pitch_plot"))),
                 tabPanel("Similarity Search",
                          sidebarPanel(),
                          mainPanel()),
                 tabPanel("Information",
                          sidebarPanel(),
                          mainPanel())
)

# Server function
server <- function(input, output, session){
  updateSelectizeInput(session = session, inputId = 'pitcher')
  
  # Pitching output and data compiling
  player_filter <- reactive({
    players %>%
      filter(full_name == input$pitcher)
  })
  
  pitch_data <- reactive({
    scrape_statcast_savant(start_date = input$dates[1],
                           end_date = input$dates[2],
                           playerid = player_filter()$id,
                           player_type = "pitcher") %>%
      mutate(description = ifelse(description == "blocked_ball", "Blocked Ball",
                                  ifelse(description == "called_strike", "Called Strike",
                                         ifelse(description %in% c("ball",
                                                                   "intent_ball"), "Ball",
                                                ifelse(description == "foul", "Foul",
                                                       ifelse(description %in% c("swinging_strike",
                                                                                 "swinging_strike_blocked"), "Swinging Strike",
                                                              ifelse(description %in% c("hit_into_play_no_out",
                                                                                        "hit_into_play_score",
                                                                                        "hit_into_play"), "Hit into Play",
                                                                     ifelse(description == "pitchout", "Pitch Out",
                                                                            ifelse(description == "foul tip", "Foul Tip",
                                                                                   ifelse(description %in% c("missed_bunt",
                                                                                                             "foul_bunt"), "Bunt Attempt", "Hit by Pitch")))))))))) %>%
      mutate(pitch_type = ifelse(pitch_type == "CU", "Curveball",
                                 ifelse(pitch_type == "SI", "Sinker",
                                        ifelse(pitch_type == "CH", "Changeup",
                                               ifelse(pitch_type == "FF", "Fastball",
                                                      ifelse(pitch_type == "PO", "Pitch out",
                                                             ifelse(pitch_type == "SL", "Slider",
                                                                    ifelse(pitch_type == "IN", "Intentional Ball", "Null"))))))))
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
          showgrid = FALSE))
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)
