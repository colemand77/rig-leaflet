library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(data.table)
library(dplyr)

#variables to be set outside the reactive loop
mapStates <- map("state", fill = TRUE, plot = FALSE)




shinyServer(function(input, output, session) {
  usedDate <- reactive({names(rigCountDates[input$dates])})
  used_Data <- reactive({getCountData(usedDate())})
  
#  thismap <- leaflet(data = mapStates) %>% addTiles() %>%
#    addPolygons(fillColor = "lightgrey", stroke = TRUE, color = "white", weight = 2) %>%
#    addPolygons(data = used_Data(), fillColor = "red", stroke = FALSE)
  
  #output$myMap <- renderLeaflet(thismap)
  output$myMap <- renderLeaflet(leaflet(data = mapStates) %>% addTiles() %>%
                                  addPolygons(fillColor = "lightgrey", stroke = TRUE, 
                                              color = "white", weight = 2) %>%
                                  addPolygons(data = used_Data(), fillColor = "red", 
                                              stroke = TRUE, color = "white", 
                                              weight = 1))
                                
  output$DateUsed <- renderText(usedDate())
  
})
# Look at leafletproxy!? might be a faster way of rendering changes to the county polygons?