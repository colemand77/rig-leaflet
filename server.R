#library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(data.table)
library(dplyr)

#variables to be set outside the reactive loop
mapStates <- map("state", fill = TRUE, plot = FALSE)

shinyServer(function(input, output, session) {
  #all the UI rendering stuff
  
  Values <- reactiveValues(oldDate = max(names(rigCountDates)))
  session$onFlush(once = FALSE, function(){
    isolate({Values$oldData <- used_Data()$names})
  })
  
  
  output$depth <- renderUI({
      checkboxGroupInput("depth","Depth",
                         choices = c("under 5k" = "<5k",
                                     "5-10k" = "5k-10k",
                                     "10-15k" = "10k-15k",
                                     "more than 15k" = ">15k",
                                     "unknown" = "N/A"),
                         selected = c("<5k","5k-10k","10k-15k",">15k","N/A"))
  })
  output$drillfor <- renderUI({ 
      checkboxGroupInput("drillfor","DrillFor",
                         choices = c("Oil" = "Oil",
                                     "Gas" = "Gas",
                                     "Miscellaneous" = "Miscellaneous"),
                         selected = c("Oil","Gas","Miscellaneous"))
  })
  output$basin <- renderUI({ 
      checkboxGroupInput("basin","Basin",
                         choices = c("Permian" = "Permian",
                                     "Williston" = "Williston",
                                     "Utica" = "Utica",
                                     "Grantite Wash" = "Granite Wash",
                                     "Haynesville" = "Haynesville",
                                     "Cana Woodford" = "Cana Woodford",
                                     "Marcellus" = "Marcellus",
                                     "DJ-Niobrara" = "DJ-Niobrara",
                                     "Eagle Ford" = "Eagle Ford",
                                     "Ardmore Woodford" = "Ardmore Woodford",
                                     "Barnett" = "Barnett",
                                     "Mississippian" = "Mississippian",
                                     "Arkoma Woodford" = "Arkoma Woodford",
                                     "Fayetteville" = "Fayetteville",
                                     "Other" = "Other"),
                         selected = c("Permian","Williston","Utica","Granite Wash",
                                      "Haynesville","Cana Woodford","Marcellus",
                                      "DJ-Niobrara", "Eagle Ford","Ardmore Woodford",
                                      "Barnett","Mississippian","Arkoma Woodford",
                                      "Fayetteville","Other"))
  })
  output$trajectory <- renderUI({ 
      checkboxGroupInput("trajectory","Trajectory",
                         choices = c("Horizontal" = "Horizontal",
                                     "Vertical" = "Vertical",
                                     "Directional" = "Directional",
                                     "Other" = "Other"),
                         selected = c("Horizontal","Vertical","Directional", "Other"))
  })
  output$welltype <- renderUI({ 
      checkboxGroupInput("welltype","Well Type",
                         choices = c("Development" = "Development",
                                     "Exploration" = "Exploration",
                                     "Infill" = "Infill",
                                     "Other" = "Other"),
                         selected = c("Development","Exploration","Infill","Other"))  
  })

  unique_names <- unique(adj[,adjName])
  
  #acutal calculation stuff
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

#define reactives to pick up UI Interface
  basins <- reactive(if(is.null(input$basin)){ c("Permian","Williston","Utica","Granite Wash",
                                                "Haynesville","Cana Woodford","Marcellus",
                                                "DJ-Niobrara", "Eagle Ford","Ardmore Woodford",
                                                "Barnett","Mississippian","Arkoma Woodford",
                                                "Fayetteville","Other")
  } else input$basin)
  depth <- reactive(if(is.null(input$depth)){
    c("<5k","5k-10k","10k-15k",">15k","N/A")
    }else input$depth)
  trajectory <- reactive(if(is.null(input$trajectory)){
    c("Horizontal","Vertical","Directional", "Other")
  } else input$trajectory)
  drillfor <- reactive(if(is.null(input$drillfor)){
    c("Oil","Gas","Miscellaneous")
  } else input$drillfor)
  welltype <- reactive(if(is.null(input$welltype)){
    c("Development","Exploration","Infill","Other")
  } else input$welltype)
  graph_group <- reactive(if(is.null(input$graph_group))"None" else input$graph_group)
  graph_stack <- reactive(if(input$stacked == "Stacked") TRUE else FALSE)

  #pick up the date from the input sheet

  usedDate2 <- reactive({input$dates})
  
  usedDate <- reactive({names(rigCountDates)[(input$date_slider)]})
  output$testdate <- renderText(usedDate())
  #output$testdate2 <- renderText(c(paste0("countyFill",mapData[[Values$oldDate]]$names))) 
  #output$testdate2 <- renderText(oldShapes())

  #subset the data as only the counties in the selected area
  used_Data <- reactive({getCountData(usedDate(), 
                                      Basin_select = basins(),
                                      Depth_select = depth(),
                                      Trajectory_select = trajectory(),
                                      DrillFor_select = drillfor(),
                                      WellType_select = welltype(),
                                      xlim = c(-130,60),#as.numeric(desc()$xlim), 
                                      ylim = c(25,50)#as.numeric(desc()$ylim)
                                      )
                         })

  used_Data2 <- reactive({
    mapData[[usedDate()]]
  })

  all_county_visible <- reactive(map("county", plot = FALSE, xlim = desc()$xlim, ylim = desc()$ylim)$names)
  
  
  #function to return the map object with the rig counts for correct date
  getCountData <- function(date, Basin_select, 
                           Depth_select, 
                           Trajectory_select,
                           DrillFor_select, 
                           WellType_select,
                           xlim = c(-130,-60), ylim = c(25,50)){
    temp <- adj[PublishDate == as.Date(date),]
    temp_County_names <- unique(temp[Country == "UNITED STATES" & 
                                       State.Province != "alaska" & 
                                       State.Province != "hawaii" &
                                       Location == "Land" &
                                       Basin %in% Basin_select &
                                       WellDepth %in% Depth_select &
                                       Trajectory %in% Trajectory_select &
                                       DrillFor %in% DrillFor_select &
                                       WellType %in% WellType_select,
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
  #input$date_slider
  #basins()
  #depth()
  #trajectory()
  #drillfor()
  #welltype()
  leaflet(data = mapStates) %>% 
    addTiles() %>%
    # setView(lat = desc()$lat, lng = desc()$lng, zoom = desc()$zoom) %>%
    addPolygons(fillColor = "lightgrey", stroke = TRUE, 
                color = "white", weight = 2) #%>%
    #addPolygons(data = isolate(used_Data2()), layerId = paste0("countyFill",names(used_Data2())), fillColor = pal(isolate(used_Data2()$count)), 
    #            fillOpacity = 0.75, stroke = TRUE, color = "white", 
    #            weight = 1, popup = as.character(isolate(used_Data2()$count)))
    })

  output$DateUsed <- renderText(usedDate())
  output$bounds <- renderText(bounds()$north)  
  output$center <- renderText(c(desc()$cent_lat, desc()$cent_lng))
  output$countyList <- renderText(used_Data()$names)
  output$choseBasin <- renderText(basins())
  output$dygraph <- renderDygraph({
    graph_rigcount(all_county_visible(), 
                   Basin = basins(), 
                   DrillFor = drillfor(),
                   Trajectory = trajectory(),
                   WellType = welltype(),
                   WellDepth = depth(),
                   group = graph_group(),
                   stacked = graph_stack())
  })


oldShapes <- reactive({setdiff(paste0("countyFill",Values$oldData),
                               paste0("countyFill",used_Data()$names))
                       })
#answer is here...
#http://stackoverflow.com/questions/26432789/can-i-save-the-old-value-of-a-reactive-object-when-it-changes
  observeEvent({input$date_slider
                basins()
                depth()
                trajectory()
                drillfor()
                welltype()
                }, {
    
    leafletProxy("myMap", deferUntilFlush = TRUE) %>% 
      removeShape(oldShapes())
    
    leafletProxy("myMap", deferUntilFlush = TRUE) %>%      
      addPolygons(data = isolate(used_Data()), layerId = paste0("countyFill",used_Data()$names), fillColor = pal(isolate(used_Data()$count)), 
                  fillOpacity = 0.75, stroke = TRUE, color = "white", 
                  weight = 1, popup = as.character(isolate(used_Data()$count)))
  }, ignoreNULL = FALSE)


})


# Look at leafletproxy!? might be a faster way of rendering changes to the county polygons?