
library(data.table)
library(maps)

load('rigcount.RData')

#adj has all the information for the releveant data
adj<- as.data.table(raw)
adj[, `:=`(PublishDate = as.Date(PublishDate, format = "%m/%d/%Y"),
           State.Province = tolower(State.Province),
           County = tolower(County))]
adj[County == "st. martin", County := "st martin:north"]
adj[, `:=`(region = State.Province,
          subregion = County, 
          adjName = paste(adj$State.Province, adj$County, sep = ","))]

rig_County_names <- unique(adj[Country == "UNITED STATES" & State.Province != "alaska" & Locatin == "Land" & State.Province != "hawaii"
                               ,adjName])

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
                                         adjName])
  tempCountyMap <- map("county", region = temp_County_names,
                      plot=FALSE, exact = TRUE, fill = TRUE)
  return(tempCountyMap)
}