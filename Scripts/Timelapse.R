setwd("/home/sweetkevindan/Desktop/images")

findDateRange <- function(x,y){
  latandlong[latandlong$`Case Date Time` >= x & latandlong$`Case Date Time` <= y,]
}
library(lubridate)
library(ggmap)
library(ggplot2)

orlando_map <- get_map(location = "Orlando", maptype = "roadmap", zoom = 11)

weeks <- seq.int(1,83)

yr1 = (2009 - 1)

for(i in weeks){
  
  mon1 <- if(i %% 12 == 0) 12 else i %% 12
  yr1 <- if(i %% 12 == 1) yr1 + 1 else yr1
  
  if(mon1 == 1 | mon1 == 3 | mon1 == 5 | mon1 == 7 | mon1 == 8 | mon1 ==10 | mon1 == 12){
    day1 <- 31
  }else if(mon1 == 4 | mon1 == 6 | mon1 == 9 | mon1 == 11){
    day1 <- 30
  }else if(leap_year(yr1)){
    day1 <- 29
  }else
    day1 <- 28

  dateStart <- paste(yr1, mon1, "1", sep = "-")
  dateFin <- paste(yr1,mon1,day1, sep = "-")
  
  Months <- findDateRange(dateStart,dateFin)
  month.coords <- subset(Months, select = c("Latitude", "Longitude"))
  
  map <- ggmap(orlando_map) + geom_point(aes(x=Longitude, y=Latitude), alpha = 0.5, size =  4, data = month.coords)
  map <- map + ggtitle(paste(mon1,yr1, sep = "-"))
  
  fn <- paste("wholedatamapmonthly",".png",sep = toString(i))
  png(filename = fn, width = 1280, height = 1280)
  print(map)
  dev.off()
  
}