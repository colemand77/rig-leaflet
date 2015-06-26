unique_dates <- unique(adj$PublishDate)

unique_dates
mapData <- lapply(unique_dates, function(x) getCountData(x))
names(mapData) <- unique_dates



unique_Basin <- unique(adj$Basin)
unique_Depth <- unique(adj$WellDepth)
unique_Trajectory <- unique(adj$Trajectory)
unique_DrillFor <- unique(adj$DrillFor)
unique_WellType <- unique(adj$WellType)

getCountData <- function(date, Basin_select = unique_Basin, 
                         Depth_select = unique_Depth, 
                         Trajectory_select = unique_Trajectory,
                         DrillFor_select = unique_DrillFor, 
                         WellType_select = unique_WellType,
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