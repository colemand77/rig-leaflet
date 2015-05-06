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
  zoom <- reactive({input$myMap_zoom})
  bounds <- reactive({input$myMap_bounds})

  
  
#  thismap <- leaflet(data = mapStates) %>% addTiles() %>%
#    addPolygons(fillColor = "lightgrey", stroke = TRUE, color = "white", weight = 2) %>%
#    addPolygons(data = used_Data(), fillColor = "red", stroke = FALSE)
  
output$myMap <- renderLeaflet(leaflet(data = mapStates) %>% addTiles() %>%
                                  addPolygons(fillColor = "lightgrey", stroke = TRUE, 
                                              color = "white", weight = 2) %>%
                                  addPolygons(data = used_Data(), fillColor = pal(used_Data()$count), 
                                              fillOpacity = 0.75, stroke = TRUE, color = "white", 
                                              weight = 1, popup = as.character(used_Data()$count)))# %>%
                                #setView(lng = input$myMap_center$lng, lat = input$myMap_center$lat, zoom = zoom()))

  output$DateUsed <- renderText(usedDate())
  output$zoom <- renderText(zoom())
  output$bounds <- renderText(bounds()$north)

  desc <- reactive({
    if(is.null(input$myMap_bounds))
      return(list())
    list(
      lat = mean(c(input$myMap_bounds$north, input$myMap_bounds$south)),
      lng = mean(c(input$myMap_bounds$west, input$myMap_bounds$east)),
      zoom = input$myMap_zoom
    )
  })
  output$center <- renderText(c(desc()$lat, desc()$lng))


})


# Look at leafletproxy!? might be a faster way of rendering changes to the county polygons?