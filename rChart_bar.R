require(devtools)
#install_github('ramnathv/rCharts')
require(rCharts)
library(data.table)

#Group <- "DrillFor"
#x_axis <- "State.Province"
#as.Date("2011-02-04")

rBarChart <- function(Group, x_axis, SelecteDate){
  GroupCol <- c(Group, x_axis)
  
  setkey(adj, PublishDate)     
  chartInput <- adj[.(SelectedDate), .(Total = sum(.SD)), .SDcols = "RigCount", by = GroupCol]
  
  Group_list<-c(unique(chartInput[,.SD, .SDcols = Group]))
  x_axis_list <- c(unique(chartInput[,(.SD), .SDcols = x_axis]))
  
  all_group <- data.table(do.call(expand.grid, 
          setNames(list(Group_list[[1]], x_axis_list[[1]]), c(Group, x_axis))))
  
  chartInput2<-merge(all_group, chartInput, by = c(Group, x_axis), all = TRUE)
  for (i in seq_along(chartInput2)) set(chartInput2, i = which(is.na(chartInput2[[i]])), j = i, value = 0)
  
  stackChart <- nPlot(x = x_axis, y = "Total", group = Group,
                      data = chartInput2, type = "multiBarChart")
  return(stackChart)
}

#rBarChart("DrillFor","State.Province",as.Date("2011-02-04"))
