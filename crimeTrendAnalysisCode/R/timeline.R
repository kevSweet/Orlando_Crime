#plots the crime over time for each 150 day period
timeline <- function(lmat,avgmat,x,y){
  
  #Garland Ave and Church St
  oneIndex <- integer(length = 2406)
  #grab all data points for one index (x,y) on crime map
  for(i in 1:2406){
    oneIndex[i] <- lmat[[i]][x,y]
  }
  oneIndex <- as.data.frame(a)
  oneIndex <- cbind(a, seq(1,2406,1))
  colnames(oneIndex) <- c("Crime", "Timeframe")
  plot(oneIndex$Crime, xlab = "Timeframe Day", ylab = "Crimes in 150 day Timeframe")
  abline(h=avgmat[x,y], col = "blue")
  text(0,avgmat[x,y]/2, "1/1/2009")
  text(2400,avgmat[x,y]/2, "12/31/2015")
  text(25, avgmat[x,y] + (avgmat[x,y]/8), "Average Line")
  
}