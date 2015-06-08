library(shiny)
library(markdown)
library(leaflet)

source("rigData.R")
source("graph_function.R")
source("ggvis_barChart.R")

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
                                        
                      h4(textOutput("DateUsed")),  
                      
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
    ),
    tabPanel("BreakDown",
             fluidPage(
               hr(),
               
              fluidRow(
                column(width =1),
                column(width = 4,
                    sliderInput("date_slider2", label = "Date Selector (historical -> Current)",
                                min = 1, max = length(names(rigCountDates)),
                                value = length(names(rigCountDates)),
                                step = 1,
                                animate = animationOptions(interval = 500))
                     ),
              column(width = 2, offset = 1,
                     selectInput("group", label = "Group Bars By",
                                 choices = list(
                                   "Country" = "Country",
                                   "Basin" = "Basin",
                                   "DrillFor" = "DrillFor",
                                   "Location" = "Location",
                                   "Trajectory" = "Trajectory",
                                   "WellType" = "WellType",
                                   "WellDepth" = "WellDepth",
                                   "State" = "State.Province",
                                   "region" = "region"),
                                 selected = "Basin")
                     ),
              column(width = 2, offset = 1,
                     selectInput("x_axis", label = "x axis",
                                 choices = list(
                                   "Country" = "Country",
                                   "Basin" = "Basin",
                                   "DrillFor" = "DrillFor",
                                   "Location" = "Location",
                                   "Trajectory" = "Trajectory",
                                   "WellType" = "WellType",
                                   "WellDepth" = "WellDepth",
                                   "State" = "State.Province",
                                   "region" = "region"),
                                 selected = "DrillFor")
              )
             ),
             fluidRow(
               column(width = 1),
               column(width = 10,
              # textOutput("DateUsed_Bar"))
               ggvisOutput("GroupedBarChart")),
              column(width = 1))
         )
    ))
  )
        
    
    
 