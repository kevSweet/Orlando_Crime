#loads in functions from .R scripts

source("/home/sweetkevindan/Desktop/git/GridTracking/R/latlongtoNM.R")
source("/home/sweetkevindan/Desktop/git/GridTracking/R/timeline.R")
source("/home/sweetkevindan/Desktop/git/GridTracking/R/differences.R")
source("/home/sweetkevindan/Desktop/git/GridTracking/R/findMax.R")
source("/home/sweetkevindan/Desktop/git/GridTracking/R/getSparseList.R")
source("/home/sweetkevindan/Desktop/git/GridTracking/R/matrixovertime.R")


library(lubridate)
library(readr)
library(anytime)
library(NISTunits)
library(Matrix)
library(png)

#OBJECTIVE: Load in data and format to distance instead of coordinates

#read in data 
matData <- read_csv("/home/sweetkevindan/Desktop/git/GridTracking/data/OPD_CrimesParsed_NoUnmapped.csv", col_types = cols(`Case Date Time` = col_datetime(format = "%m/%d/%Y %H:%M")))

#select only relevant columns, also converts Datetime to Unix Time
matData <- data.matrix(subset(matData, select = c("Latitude", "Longitude","Case Date Time")))

#defines center of orlando as datum
orlcenter <- c(28.5383,-81.3792)

#finds radius of the earth at that point
rad <- findradius(orlcenter)

#converts lat/long to a vector of nm in x and y from center of orlando
for (i in 1:nrow(matData)){
  temp <- converttognomonic(rad, cbind(matData[i,1],matData[i,2]), orlcenter)
  matData[i,1] <- temp[1]
  matData[i,2] <- temp[2]
}

colnames(matData) <- c("Y(nm)", "X(nm)", "Time(Unix)")

#OBJECTIVE: Map each point onto a sparse matrix in order to create a heatmap of multiple timeframes

#sets the datum as the upper left corner of the dataset
matData[,1] <- max(matData[,1]) - matData[,1]
matData[,2] <- matData[,2] - min(matData[,2])

#compensates for min and max edgecases in which the distance from datum would be indexed at 0
matData[,1] <- matData[,1] + 0.00000001
matData[,2] <- matData[,2] + 0.00000001


#bin sizing: 0.2 X 0.2 nm in size. Each index of matrix represents 0.2 square nautical miles of orlando
bs <- 0.2

#rounds each data point to the nearest matrix index
matData[,1] <- ceiling(matData[,1]/bs)*bs
matData[,2] <- ceiling(matData[,2]/bs)*bs

#defines map size as a whole in (i,j)
idimsize <- max(matData[,2])/bs
jdimsize <- max(matData[,1])/bs

startTime <- min(matData[,3])

#adds one in order to round all days up to 1
matData[,3] <- matData[,3] - startTime + 1
#86400 is 1 day in seconds converts unix time into days from startTime
matData[,3] <- ceiling(matData[,3]/86400)

colnames(matData) <- c("Y(nm)", "X(nm)","Time(Days from startTime)")
#declare a list to store sparse matrices
l <- vector("list", nrow(matData))

#create a sparse matrix for each point in dataset then form a list in order to look at datapoints in different timeframes
#fill list with matrices
for(x in 1:nrow(matData)){
  l[x] <- sparseMatrix(i=matData[x,2]/bs, j = matData[x,1]/bs, dims = c(idimsize,jdimsize))
}


fullMatrixList <- vector("list")
rm(l,i,idimsize,jdimsize,orlcenter,bs,temp,rad,startTime)

#call from script matrixovertime.R
fullMatrixList <- crimeMatrixOverTime(l, matData)

#call from script differences.R
avgmat <- generateDiffImages(fullMatrixList)


#call from script timeline.R 

#South ST and Garland Avenue plot
timeline(fullMatrixList,avgmat,22, 34)

#other interesting data points
timeline(fullMatrixList,avgmat,46,12)
timeline(fullMatrixList,avgmat,47,10)
timeline(fullMatrixList,avgmat,11,31)

#airport plot over time
timeline(fullMatrixList,avgmat,55,52)
