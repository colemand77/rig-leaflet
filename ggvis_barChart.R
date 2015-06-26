library(data.table)
library(ggvis)
library(dplyr)


ggvisDataPrep <- function(Group, x_axis, SelectedDate){
  GroupCol <- c(Group, x_axis)
  
  setkey(adj, PublishDate)     
  chartInput <- adj[.(as.Date(SelectedDate)), .(Total = sum(.SD)), .SDcols = "RigCount", by = GroupCol]
  
  Group_list<-c(unique(chartInput[,.SD, .SDcols = Group]))
  x_axis_list <- c(unique(chartInput[,(.SD), .SDcols = x_axis]))
  
  all_group <- data.table(do.call(expand.grid, 
                                  setNames(list(Group_list[[1]], x_axis_list[[1]]), c(Group, x_axis))))
  
  chartInput2<-merge(all_group, chartInput, by = c(Group, x_axis), all = TRUE)
  for (i in seq_along(chartInput2)) set(chartInput2, i = which(is.na(chartInput2[[i]])), j = i, value = 0)
  chartInput2[,RowId:=(1:nrow(chartInput2))]

  return(chartInput2)
  
}

create_Chart <- function(barChart_data, Group, x_axis){
  class(barChart_data) <- "data.frame"  

  toolTip_data <- function(x){
    if(is.null(x)) return(NULL)
    row <- barChart_data[barChart_data$RowId == x$RowId, ]
    #print(row)
    
    paste0(paste0(names(x)[1], ": ", format(x)[1], collapse = "<br />"),
           "<br />",
           paste0("total: ", format(x$stack_upr_ - x$stack_lwr_)))
  }
  #print(barChart_data)
  
  stackChart <- barChart_data %>%
      ggvis(prop("x", as.name(x_axis)), y = ~Total, prop("fill", as.name(Group))) %>%
      layer_bars(stack = TRUE, stroke := "white") %>%
      add_tooltip(toolTip_data, "hover") %>%
      add_axis("x", properties = axis_props(
        labels = list(angle = 45, align = "left"))) %>%
      set_options(width = "auto")
  return(stackChart)
}

ggvisBarChart <- function(Group, x_axis, SelectedDate){
  data2 <- ggvisDataPrep(Group, x_axis, SelectedDate)
  chart<-create_Chart(data2, Group, x_axis)
  return(chart)
}



#input_a <- "WellType"
#input_b <- "Trajectory"

#ggvisDataPrep(input_a, input_b, as.Date("2011-03-11"))
#ggvisBarChart(input_a, input_b, as.Date("2011-03-11"))
#ata %>% ggvis(x = ~WellType, y = ~Total) %>%
#  layer_bars(stack = TRUE, fill = ~factor(Trajectory))
