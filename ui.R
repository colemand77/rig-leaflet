library(shiny)
library(leaflet)
library(markdown)
source("rigData.R")
source("graph_function.R")

basinList <- unique(adj[,Basin])

shinyUI(
  navbarPage("RigExplorer", id ="nav",
    tabPanel("Interactive Map",
      div(class = "outer",
          tags$head(
            includeCSS("styles.css")),
          
        leafletOutput("myMap", width = "100%", height = "100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      h2("Rig Count Explorer"),
                      
                    #  h5("Date Used"),
                    #  h5(textOutput("DateUsed")),
                    #  sliderInput("dates", label = h3("Date Selector"),
                    #              min = min(rigCountDates), 
                    #             max = max(rigCountDates), 
                    #              value = max(rigCountDates),
                    #              animate = TRUE),
                      
                      selectInput("dates", label = "Select Date of Report",
                                  choices = names(rigCountDates),
                                  selected = max(names(rigCountDates))),
                    
                      sliderInput("date_slider", label = "test",
                                  min = 0, max = length(names(rigCountDates)),
                                  value = max(length(rigCountDates)),
                                  step = 1,
                                  animate = animationOptions(interval = 200)),
                      textOutput("testdate"),
                      textOutput("testdate2"),
                      
                      radioButtons("details", "Show Breakdown:",
                                   c("All" = "True",
                                     "None" = "False"),
                                   selected = "False"),
                      
                      conditionalPanel(
                        condition = "input.details == 'True'",
                        uiOutput("basin"),
                        uiOutput("depth"),
                        uiOutput("drillfor"),
                        uiOutput("trajectory"),
                        uiOutput("welltype"))
                                            
                      ),
        
        absolutePanel(id = "graph", class = "panel panel-default", fixed = FALSE,
                      draggable = FALSE, top = "auto", left = 20, right = "auto", bottom = 5,
                      #width = 600, #height = 400,
                      h4("graph output"),
                      selectInput("graph_group", label = "",
                                  choices = list(
                                    "Basin" = "Basin",
                                    "Drill For" = "DrillFor",
                                    "Trajectory" = "Trajectory",
                                    "Well Type" = "WellType",
                                    "Well Depth" = "WellDepth",
                                    "none" = "none"),
                                  selected = "none"),
                      radioButtons("stacked", label = NULL,
                                   c("Stacked" = "Stacked",
                                     "Linear" = "Linear"),
                                   selected = "Linear", inline = TRUE),
                      dygraphOutput("dygraph", width = "500px", height = "500px")
                      )
      )
    )#,
    #tabPanel("instruction",
    #         fluidPage(
    #           includeMarkdown("Instructions.html")
    #           ))
  )
)
        
    
    
 