library(shiny)
library(markdown)
library(leaflet)

source("rigData.R")
source("graph_function.R")

basinList <- unique(adj[,Basin])

shinyUI(
  navbarPage("RigExplorer", id ="nav",
    tabPanel("Interactive Map",
      div(class = "container-fluid",
          tags$head(
            includeCSS("styles.css")),
          
        leafletOutput("myMap", width = "100%", height = "100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, 
                      h2("Rig Count Explorer"),
                                        
                      h3(textOutput("DateUsed")),  
                      
                      sliderInput("date_slider", label = "Date Selector (historical -> Current)",
                                  min = 1, max = length(names(rigCountDates)),
                                  value = length(names(rigCountDates)),
                                  step = 1,
                                  animate = animationOptions(interval = 750)),
                      
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
                      dygraphOutput("dygraph")#, width = "auto", height = "auto") #"500px"
                      )
      )
    )#,
    #tabPanel("instruction",
    #         fluidPage(
    #           includeMarkdown("Instructions.html")
    #           ))
  )
)
        
    
    
 