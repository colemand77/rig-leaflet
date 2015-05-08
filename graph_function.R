library(dygraphs)
library(xts)

ts_rigcount <- function(countynames){
  temp <- adj[adjName %in% countynames,.(RigCount = sum(RigCount)), by = .(PublishDate)]
  as.xts(temp$RigCount, order.by = temp$PublishDate)
}


samplenames <- c("texas,andrews","texas,brazoria","texas,brooks","texas,crockett")
sampledata<- ts_rigcount(samplenames)

graph_rigcount <- function(countynames){
  dygraph(ts_rigcount(countynames)) %>% dyRangeSelector()
}

fun1 <- function(a = 3, b = 5, ...){
  lapply(list(...), function(x){
    return(names(x))    
  })
}

fun1(,one = 3, four = 5)
