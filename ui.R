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
        checkboxGroupInput("basin_group",
          label = h3("Basins"),
          choices = list("Permian" = "Permian",
                         "Utica" = "Utica",
                         "Granite Wash" = "Granite Wash",
                         "Haynesville" = "Haynesville",
                         "Cana Woodford" = "Cana Woodford",
                         "Marcellus" = "Marcellus",
                         "DJ-Niobrara" = "DJ-Niobrara",
                         "Eagle Ford" = "Eagle Ford",
                         "Ardmore Woodford" = "Ardmore Woodford",
                         "Barnett" = "Barnett",
                         "Mississippian" = "Mississippian",
                         "Williston" = "Williston",
                         "Arkoma Woodford" = "Arkoma Woodford",
                         "Fayetteville" = "Fayetteville",
                         "Other" = "Other"),
          selected = c("Permian","Utica","Granite Wash","Haynesville","Cana Woodford",
                       "Marcellus","DJ-Niobrara","Eagle Ford","Ardmore Woodford",
                       "Barnett","Mississippian","Williston","Arkoma Woodford",
                       "Fayetteville","Other")
        ),
        
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

