# User interface
ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "An Interactive Baseball Visualization Application",
                 tabPanel("Spray Chart",
                          sidebarPanel(),
                          mainPanel()),
                 tabPanel("Pitching Chart",
                          sidebarPanel(
                            selectizeInput(inputId = "pitcher",
                                           choices = players$full_name,
                                           label = "Select pitcher",
                                           selected = NULL)
                          ),
                          mainPanel(plotlyOutput(outputId = "pitch_plot"))),
                 tabPanel("Similarity Search",
                          sidebarPanel(),
                          mainPanel()),
                 tabPanel("Information",
                          sidebarPanel(),
                          mainPanel())
)