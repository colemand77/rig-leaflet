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
        conditionalPanel(
          condition = "'Basin_select' %in% input.UI_Checkbox",
          uiOutput("basin")),
        
        conditionalPanel(
          condition = "'Depth_select' %in% input.UI_Checkbox",
          uiOutput("depth")),
        
        conditionalPanel(
          condition = "'DrillFor_select' %in% input.UI_Checkbox",
          uiOutput("drillfor")),
        
        conditionalPanel(
          condition = "'Trajectory_select' %in% input.UI_Checkbox",
          uiOutput("trajectory")),
        
        conditionalPanel(
          condition = "'WellType_select' %in% input.UI_Checkbox",
          uiOutput("welltype")),
        
        textOutput("DateUsed"),
        textOutput("choseBasin"),
        textOutput("countyList")      
        
        ),
      mainPanel(
               leafletOutput("myMap"),
               dygraphOutput("dygraph")
      )
      )
  )
)

