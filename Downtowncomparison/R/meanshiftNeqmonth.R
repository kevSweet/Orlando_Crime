library(MeanShift)
library(ggplot2)
library(ggmap)
library(lubridate)


OPD_CrimesParsed_NoUnmapped <- read.csv("OPD_CrimesParsed_NoUnmapped.csv") 
setwd("./images")

#subsets downtownlatandlong in between dates x and y as strings
findDateRange <- function(x,y){
    downtownlatandlong[downtownlatandlong$`Case Date Time` >= x & downtownlatandlong$`Case Date Time` <= y,]
}

  #get map of Orlando
  orlando_map <- get_map(location = "Orlando", maptype = "roadmap", zoom = 14)

  #defines latandlong as columns of opd crimes offense type, lat, and long
  latandlong <- subset(OPD_CrimesParsed_NoUnmapped, select = c("Case.Date.Time","Case.Offense.Type", "Latitude", "Longitude"))
  
  #narrows dataset to downtown
  downtownlatandlong <- subset(latandlong, Latitude > 28.514520 & Latitude < 28.561178 & Longitude > -81.401205 & Longitude < -81.352166)

  #initializes months
  Months <- findDateRange("2009-1-1","2009-2-28")
  month.coords <- subset(Months, select = c("Latitude","Longitude"))

  #performs mean shift clustering with bandwidth h
  ms <- msClustering(t(month.coords),h=.00275,kernel = "gaussianKernel",multi.core = TRUE)
  
  #ms returned 67 clusters with h value of .00275 (gauss kernel)
  km <- kmeans(month.coords, centers = 67)
  
  df <- data.frame(latitude = km$centers[,1], longitude = km$centers[,2])
  map <- ggmap(orlando_map, extent = "device") + geom_point(aes(x=Longitude, y=Latitude), col = km$cluster, alpha = 0.3, size =  4, data = month.coords)
  map <- map + geom_point(aes(x = longitude, y= latitude), shape = "+", data = df, size = 18, alpha = 0.65)
  map <- map + ggtitle("1 2009 - 2 2009")
  
  fn <- "map1.png"
  png(filename = fn, width = 1280, height = 1280)
  print(map)
  dev.off()
  
  #number of months left in dataset
  months <- seq.int(2,83)
  yr1 <- 2009
  yr2 <- 2009
  
for (month in months){
  
  mon1 <- if(month %% 12 == 0) 12 else month %% 12
  mon2 <- if((month+1) %% 12 == 0) 12 else (month+1) %% 12
  yr1 <- if(month %% 12 == 1) yr1 + 1 else yr1
  yr2 <- if((month+1) %% 12 == 1) yr2 + 1 else yr2
  
  if(mon2 == 1 | mon2 == 3 | mon2 == 5 | mon2 == 7 | mon2 == 8 | mon2 ==10 | mon2 == 12){
    day2 <- 31
  }else if(mon2 == 4 | mon2 == 6 | mon2 == 9 | mon2 == 11){
    day2 <- 30
  }else if(leap_year(yr2)){
    day2 <- 29
  }else
    day2 <- 28
  
  dateStart <- paste(yr1, mon1, "1", sep = "-")
  dateFin <- paste(yr2, mon2, day2, sep = "-")
  
  
  Months <- findDateRange(dateStart,dateFin)
  
  month.coords <- subset(Months, select = c("Latitude", "Longitude"))
  
  km <- kmeans(month.coords, centers = 67)
  df <- data.frame(latitude = km$centers[,1], longitude = km$centers[,2])
  
  map <- ggmap(orlando_map, extent = "device") + geom_point(aes(x=Longitude, y=Latitude), col = km$cluster, alpha = 0.3, size =  4, data = month.coords)
  map <- map + geom_point(aes(x = longitude, y= latitude), shape = "+", data = df, size = 18, alpha = 0.65)
  map <- map + ggtitle(paste(mon1,yr1,"-",mon2,yr2))
  
  fn <- paste("map",".png",sep = toString(month))
  png(filename = fn, width = 1280, height = 1280)
  print(map)
  dev.off()
  
}

