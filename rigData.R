library(leaflet)
library(data.table)
library(maps)

load('rigcount.RData')
bluecolors <- c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
pal <- colorBin(bluecolors, domain = NULL)


#adj has all the information for the releveant data
adj<- as.data.table(raw)
adj[, `:=`(PublishDate = as.Date(PublishDate, format = "%m/%d/%Y"),
           State.Province = tolower(State.Province),
           County = tolower(County))]
adj[State.Province == "louisiana" & County == "st. martin", County := "st martin:north"]
adj[State.Province == "texas" & County == "galveston", County := "galveston:main"]
adj[State.Province == "florida" & County == "okaloosa", County := "okaloosa:main"]
adj[State.Province == "north carolina" & County == "currituck", County :=" currituck:main"]
adj[State.Province == "virginia" & County == "accomack", County := "accomack:main"]
adj[State.Province == "washington" & County == "pierce", County := "pierce:main"]
adj[State.Province == "washington" & County == "san juan", County := "san juan:san juan island"]
set(adj, i = NULL, j = "County", value = gsub(".","",adj[["County"]],fixed = TRUE))
# this is roughly 2x faster:  system.time(set(adj, i = which(adj[["State.Province"]] == "washington" & adj[["County"]] == "san juan"), "County", "san juan:san juan island"))
adj

adj[, `:=`(region = State.Province,
          subregion = County, 
          adjName = paste(adj$State.Province, adj$County, sep = ","))]

rig_County_names <- unique(adj[Country == "UNITED STATES" & 
                                 State.Province != "alaska" & 
                                 Location == "Land" & 
                                 State.Province != "hawaii",
                               adjName])

rigCountDates <- 1:length(unique(adj[,PublishDate]))
names(rigCountDates) <-  unique(adj[,PublishDate])

#note I used approximate matching here. THis could be a problem going forward.
RigCountyMap <- map("county", region  = rig_County_names,
                 plot=FALSE, exact = FALSE, fill = TRUE)

getCountData <- function(date){
  temp <- adj[PublishDate == as.Date(date),]
  temp_County_names <- unique(temp[Country == "UNITED STATES" & 
                                     State.Province != "alaska" & 
                                     State.Province != "hawaii" &
                                     Location == "Land",
                                         .(count = sum(RigCount)),by = adjName])
  tempCountyMap <- map("county", region = temp_County_names$adjName,
                      plot=FALSE, exact = TRUE, fill = TRUE)
  tempCountyMap$count <- temp_County_names$count
  #tempCountyMap$col <- pal(temp_County_names$RigCount)
  return(tempCountyMap)
}


#test code

#adj[adjName == getCountData("2015-04-24")$names]
#test<-unique(adj[Country == "UNITED STATES" & 
#              State.Province != "alaska" & 
#              State.Province != "hawaii" &
#              Location == "Land",
#            .(adjName,sum(RigCount)), by = .(adjName)])
#test

#which(test$adjName %in% "louisiana,st martin:north")

#str(getCountData("2015-04-24"))
#is.numeric(getCountData("2015-04-24")$count)

#to find out which countiesare not showing up
#lapply(test$adjName, function(x) {
#  print(map("county", region = x, exact = TRUE, plot = FALSE))
#})

