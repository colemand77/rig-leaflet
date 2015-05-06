library(shiny)
library(leaflet)
source("rigData.R")

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
        textOutput("DateUsed"),
        textOutput("zoom"),
        textOutput("bounds"),
        textOutput("center")
        
        ),
      mainPanel(
               leafletOutput("myMap")
      )
      )
  )
)

