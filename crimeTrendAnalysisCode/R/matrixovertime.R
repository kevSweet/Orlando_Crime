
crimeMatrixOverTime <- function(l,matData){
#load in prep matrix function
source("/home/sweetkevindan/Desktop/git/GridTracking/R/prepMatrix.R")

horiznTime <- 150 
shift <- 1

#Objective: Find the maximum number of crimes in a 150 day timeframe and use that to scale the matrix from 0,1

#only need to calculate maxinDat/diffmaxinDat once, then hardcode to save time when running
#Dat <- findmax(matData, l, horiznTime)
#maxinDat <- Dat[1]
#diffmaxinDat <- Dat[2]
maxinDat <- 458
diffmaxinDat <- 21

setwd("/home/sweetkevindan/Desktop/git/GridTracking/output/Changeovertime/")
#initializes timeframe, necessary because loop adds and subtracts the prior and current timeframe
ctime <- 1 
timeframe <- l[matData[,3] < ctime + horiznTime & matData[,3] >= ctime]
smat <- Reduce("+", timeframe)
finmat <- prepMatrix(smat, maxinDat)
fn <- paste("day", toString(ctime),".png", sep = "")
writePNG(1 - finmat, target = fn)

#initializes the list used when finding the differenes from the average (input to differences.R)
lmat <- vector("list", length = max(matData[,3]) - horiznTime)
lmat[[1]] <- as.matrix(smat)

#loop over every timeframe and calculate the matrix over each timeframe and the change in matrix over time
for(ctime in 2:(max(matData[,3]) - horiznTime)){
  
  #adding the next day to timeframe, subtracting last day from timeframe
  ad <- l[matData[,3] == ctime + horiznTime - 1]
  sb <- l[matData[,3] == ctime - shift]

  #if no crimes in either the day you add or subtract, skip to next day
  if (is.null(Reduce("+", ad)) & is.null(Reduce("+", sb))){
   #donothing

  #if there is no crime in the day you are trying to add, only subtract the previous day
  }else if (is.null(Reduce("+", ad))){
    smat <- smat - Reduce("+",sb)
    sdifmat <- Reduce("+", sb)
  #if there is no crime in the day you are trying to subtract, only add the next day
  }else if (is.null(Reduce("+", sb))){
    smat <- smat + Reduce("+", ad)
    sdifmat <- Reduce("+", ad)
  }else{
    smat <- smat + Reduce("+", ad) - Reduce("+",sb)
    sdifmat <- Reduce("+", ad) + Reduce("+", sb)
  }
  
  
  #convert to full matrix and scale, stores that entry in list (for differences.R), and converts the change in matrix over time to matrix and scale
  finmat <- prepMatrix(smat, maxinDat)
  lmat[[ctime]] <- as.matrix(smat)
  difmat <- prepMatrix(sdifmat, diffmaxinDat)

  #sets print directory for d/dt matrix, prints to loc
  setwd("/home/sweetkevindan/Desktop/git/GridTracking/output/differenceinchangeovertime/")
  fn <- paste("day", toString(ctime),".png", sep = "")
  writePNG(1 - difmat, target = fn)

  #sets print directory for reg matrix, prints to loc
  setwd("/home/sweetkevindan/Desktop/git/GridTracking/output/Changeovertime/")
  fn <- paste("day", toString(ctime),".png", sep = "")
  writePNG(1 - finmat, target = fn)
}
#return
return(lmat)
}


