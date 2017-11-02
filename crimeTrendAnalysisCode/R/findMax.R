#finds max number of crimes in any timeframe as well as the max difference in any timeframe
findmax <- function(matData, l, horiznTime){
  library(Matrix)
  #determines max number of points in matrix
  ctime <- 1
  timeframe <- l[matData[,3] < ctime + horiznTime & matData[,3] >= ctime]
  smat <- Reduce("+", timeframe)
  mat <- as.matrix(smat)
  maxinDat <- max(mat)
  diffmaxinDat <- 0
  
  for(ctime in 2:(max(matData[,3]) - horiznTime)){
    add <- l[matData[,3] == ctime + horiznTime]
    sub <- l[matData[,3] == ctime - 1]
    
    sdiffr <- Reduce("+", add) + Reduce("+", sub)
    smat <- smat + Reduce("+", add) - Reduce("+",sub)
    
    diffr <- as.matrix(sdiffr)
    mat <- as.matrix(smat)
    
    if(max(diffr) > diffmaxinDat){
      diffmaxinDat <- max(diffr)
    }
    if(max(mat) > maxinDat){
      maxinDat <- max(mat)
    }
    
  }
  findmax <- c(maxinDat, diffmaxinDat)
}
