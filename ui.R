library(shiny)
library(leaflet)
source("rigData.R")
source("graph_function.R")
basinList <- unique(adj[,Basin])

shinyUI(
  fluidPage(
    titlePanel("my Shiny App"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("dates", label = h3("Sliders"),
                    min = min(rigCountDates), 
                    max = max(rigCountDates), 
                    value = max(rigCountDates),
                    animate = TRUE),
        
        checkboxGroupInput("UI_Checkbox", 
                           label = "Filter By:",
                           choices = list(
                             "Basin Selector" = "Basin_select", 
                             "Well Depth Selector" = "Depth_select",
                             "Drilling For " = "DrillFor_select",
                             "Direction Type" = "Trajectory_select",
                             "Well Type" = "WellType_select"
                           )),
# to get it to work better on the shiny server        
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
          uiOutput("welltype")),
            
        textOutput("DateUsed")
        #textOutput("choseBasin"),
       # textOutput("countyList")      
        
        ),
      mainPanel(
               leafletOutput("myMap"),
               selectInput("graph_group", label = "group chart by",
                           choices = list(
                             "Basin" = "Basin",
                             "Drill For" = "DrillFor",
                             "Trajectory" = "Trajectory",
                             "Well Type" = "WellType",
                             "Well Depth" = "WellDepth",
                             "none" = "none"),
                           selected = "none"),
               dygraphOutput("dygraph")
      )
      )
  )
)

