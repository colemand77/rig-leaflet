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

  #desc returns the details of the active map frame / center / zoom limits
  desc <- reactive({
    if(is.null(input$myMap_bounds)){
      list(
        ylim = c(25,50),
        xlim = c(-130,-60))
      } else {
    list(
      cent_lat = mean(c(input$myMap_bounds$south, input$myMap_bounds$north)),
      cent_lng = mean(c(input$myMap_bounds$west, input$myMap_bounds$east)),
      bounds = input$myMap_bounds,
      zoom = input$myMap_zoom,
      ylim = c(input$myMap_bounds$south, input$myMap_bounds$north),
      xlim = c(input$myMap_bounds$west, input$myMap_bounds$east)
      )}
  })
  basins <- reactive(input$basin_group)
  
  #pick up the date from the input sheet
  usedDate <- reactive({names(rigCountDates[input$dates])})
  #subset the data as only the counties in the selected area
  used_Data <- reactive({getCountData(usedDate(), Basin_select = basins(), 
                                      xlim = as.numeric(desc()$xlim), 
                                      ylim = as.numeric(desc()$ylim))})
  all_county_visible <- reactive(map("county", plot = FALSE, xlim = desc()$xlim, ylim = desc()$ylim)$names)
  
  
  #function to return the map object with the rig counts for correct date
  getCountData <- function(date, Basin_select, xlim = c(-130,-60), ylim = c(25,50)){
    temp <- adj[PublishDate == as.Date(date),]
    temp_County_names <- unique(temp[Country == "UNITED STATES" & 
                                       State.Province != "alaska" & 
                                       State.Province != "hawaii" &
                                       Location == "Land" &
                                       Basin %in% Basin_select,
                                     .(count = sum(RigCount)),by = adjName])
    tempCountyMap <- map("county", region = temp_County_names$adjName,
                         plot=FALSE, exact = TRUE, fill = TRUE, 
                         xlim = xlim, ylim = ylim)
    
    tempCountyMap$count <- temp_County_names$count
    return(tempCountyMap)
  }
  
#  thismap <- leaflet(data = mapStates) %>% addTiles() %>%
#    addPolygons(fillColor = "lightgrey", stroke = TRUE, color = "white", weight = 2) %>%
#    addPolygons(data = used_Data(), fillColor = "red", stroke = FALSE)
  

#Render the leaflet map; this is only updated when the input$dates is changed,
#all other reactives are protected by isolate()
output$myMap <- renderLeaflet({
  input$dates
  input$basin_group
  leaflet(data = mapStates) %>% 
    addTiles() %>%
    # setView(lat = desc()$lat, lng = desc()$lng, zoom = desc()$zoom) %>%
    addPolygons(fillColor = "lightgrey", stroke = TRUE, 
                color = "white", weight = 2) %>%
    addPolygons(data = isolate(used_Data()), fillColor = pal(isolate(used_Data()$count)), 
                fillOpacity = 0.75, stroke = TRUE, color = "white", 
                weight = 1, popup = as.character(isolate(used_Data()$count)))
  })

  output$DateUsed <- renderText(usedDate())
  output$bounds <- renderText(bounds()$north)  
  output$center <- renderText(c(desc()$cent_lat, desc()$cent_lng))
  output$countyList <- renderText(used_Data()$names)
  output$choseBasin <- renderText(basins())
  output$dygraph <- renderDygraph({
    graph_rigcount(all_county_visible())
  })
})


# Look at leafletproxy!? might be a faster way of rendering changes to the county polygons?