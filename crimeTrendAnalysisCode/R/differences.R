#generate list of matrices
#add all matrices together
#find average map
#subtract average map from each frame
require("png")
generateDiffImages <- function(lmat){

#finds average matrix from matrices in list
tmat <- Reduce("+", lmat)
avgmat <- tmat/length(lmat)



#CALCULATE MAX AND MIN DIFFERENCE IN FOR LOOP LIKE BELOW SEE
#*****
maxinc <- 0
maxdec <- 0


for(i in 1:length(lmat)){
  
  if(maxinc < max(lmat[[i]] - avgmat)){
     maxinc <- max(lmat[[i]] - avgmat)
  }

  if(maxdec > min(lmat[[i]] - avgmat)){
      maxdec <- min(lmat[[i]] - avgmat)
  }
  
}
maxChange <- if(maxinc < maxdec) maxdec else maxinc
dmat <- vector("list", length = length(lmat))

#finds the the difference between each matrix and the average


#build rgb matrices. Higher than average crime -> red. lower than average crime -> green
for(i in 1:length(lmat)){
  Mpos <- (lmat[[i]] * ((lmat[[i]] - avgmat) < 0))/maxChange
  Rpos.mat <- ifelse(Mpos > 0, 1 - Mpos, 1)
  Gpos.mat <- array(1, c(80,74))
  Bpos.mat <- ifelse(Mpos > 0, 1 - Mpos, 1)
  
  Mneg <- (lmat[[i]] * ((lmat[[i]] - avgmat) > 0))/maxChange
  Rneg.mat <- array(1, c(80,74))
  Gneg.mat <- ifelse(Mneg > 0, 1 - Mneg, 1)
  Bneg.mat <- ifelse(Mneg > 0, 1 - Mneg, 1)
  
  final_image <- array(NA, c(80,74,4))
  final_image[,,1] <-  Rpos.mat * Rneg.mat
  final_image[,,2] <-  Gpos.mat * Gneg.mat
  final_image[,,3] <-  Bpos.mat * Bneg.mat
  final_image[,,4] <- 0.9
  #final_image[,,3] <-  ifelse(Bpos.mat + Bneg.mat == 2, Bpos.mat, Bpos.mat + Bneg.mat)
  fn <- paste("./images/deviationfromaverageovertime/day", toString(i),".png", sep = "")
  writePNG(final_image, target = fn)
}
generateDiffImages <- avgmat
}
