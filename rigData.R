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


# modify county names to be consistent with map names
changeNames <- list(c("louisiana", "st. martin", "st martin:north"), 
                    c("texas", "galveston", "galveston:main"),
                    c("florida", "okaloosa", "okaloosa:main"),
                    c("north carolina", "currituck", "currituck:main"),
                    c("virginia", "accomack","accomack:main"),
                    c("washington","pierce","pierce:main"),
                    c("washington","san juan","san juan:sanjuan"))
lapply(changeNames, function(x) {
  set(adj, i = which(adj[["State.Province"]] == x[[1]] & adj[["County"]] == x[[2]]), 
      "County", x[[3]])
  })
set(adj, i = NULL, j = "County", value = gsub(".","",adj[["County"]],fixed = TRUE))
set(adj, i = which(adj[["Basin"]] == "Dj-Niobrara"), j = "Basin", 
    value = "DJ-Niobrara")


# add the adjName column
adj[, `:=`(region = State.Province,
          subregion = County, 
          adjName = paste(adj$State.Province, adj$County, sep = ","))]
str(adj)
rig_County_names <- unique(adj[Country == "UNITED STATES" & 
                                 State.Province != "alaska" & 
                                 Location == "Land" & 
                                 State.Province != "hawaii",
                               adjName])

rigCountDates <- 1:length(unique(adj[,PublishDate]))
names(rigCountDates) <-  sort(unique(adj[,PublishDate]))


#note I used approximate matching here. THis could be a problem going forward.
#RigCountyMap <- map("county", region  = rig_County_names,
#                 plot=FALSE, exact = FALSE, fill = TRUE)

#test code
#getCountData("2015-04-24", xlim = c(-100,-90), ylim = c(35,36))
#test<-getCountData("2015-04-24", xlim = c(-100,-99), ylim = c(35,36))


#adj[adjName == getCountData("2015-04-24")$names]
#test<-unique(adj[Country == "UNITED STATES" & 
##              State.Province != "alaska" & 
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


