library(dygraphs)
library(xts)
library(tidyr)

countyList_in_scope <- function(xlim, ylim){
  map("county",xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)$names
}


groupingString <- function(group){
  if (is.null(group)) return("list(PublishDate)")
  if(group == "none") return("list(PublishDate)") else {
    paste0("list(PublishDate,", group,")")
  }
}

groupingString("Thisthing")

ts_rigcount <- function(countynames, group, ...){
  
  paramstring <- build_Criteria(...)
  temp <- adj[adjName %in% countynames & eval(parse(text = paramstring)),
              list(RigCount = sum(RigCount)), 
              by = eval(parse(text = groupingString(group)))]
  if(group != "none"){
    temp <- spread_(temp, group, "RigCount", fill = 0)
    as.xts(temp[,!"PublishDate", with = FALSE], order.by = temp$PublishDate)
  } else{
  as.xts(temp$RigCount, order.by = temp$PublishDate)
  }
}

# will have to add the build_Criteria() function at some point,
#but for now this will work
graph_rigcount <- function(countynames,...){
  dygraph(ts_rigcount(countynames,...)) %>% dyRangeSelector()
}

#way to build functions into the criteria; need to have the checkboxes
#in order to see it broken down properly
par_fmt <- function(par){
  paste0("'",par,"'", collapse = ',')
}

grp_fmt <- function(...){
  parArray <- par_fmt((...))
  argname <- names(list(...))
  paste0(argname, " %in% c(", parArray,")")
}

build_Criteria <- function(Basin = NULL, 
                           DrillFor = NULL,
                           Trajectory = NULL,
                           WellType = NULL,
                           WellDepth = NULL){
  paramlist <- c()
  if (!is.null(Basin)){  
    if(length(paramlist)==0) paramlist <- paste0(grp_fmt(Basin = Basin)) else {
      paramlist <- paste(paramlist, grp_fmt(Basin = Basin), sep = " & ")
    }
  }
  if (!is.null(DrillFor)){
    if(length(paramlist)==0) paramlist <- paste0(grp_fmt(DrillFor = DrillFor)) else {
      paramlist <- paste(paramlist, grp_fmt(DrillFor = DrillFor), sep = " & ")
    }
  }
  if (!is.null(WellDepth)){
    if(length(paramlist)==0) paramlist <- paste0(grp_fmt(WellDepth = WellDepth)) else {
      paramlist <- paste(paramlist, grp_fmt(WellDepth = WellDepth), sep = " & ")
    }
  }
  if (!is.null(Trajectory)){
    if(length(paramlist)==0) paramlist <- paste0(grp_fmt(Trajectory = Trajectory)) else {
      paramlist <- paste(paramlist, grp_fmt(Trajectory = Trajectory), sep = " & ")
    }
  }
  if (!is.null(WellType)){
    if(length(paramlist)==0) paramlist <- paste0(grp_fmt(WellType = WellType)) else {
      paramlist <- paste(paramlist, grp_fmt(WellType = WellType), sep = " & ")
    }
  }  
  return(paramlist)
}


#test <- build_Criteria(Basin = c("Eagle Ford", "Permian"), DrillFor = "Gas")
#test
#test2 <- adj[eval(parse(text = test)),.(RigCount = sum(RigCount)), by = .(PublishDate, WellDepth)]
##test2
#testinput <- "WellDepth"
#test3 <- spread_(test2, testinput, "RigCount", fill = 0)
as.xts(test3[,!"PublishDate",with = FALSE], order.by = test3$PublishDate)

