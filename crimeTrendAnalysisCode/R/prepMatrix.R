
#OBJECTIVE: format matrix in order to scale it for image output
prepMatrix <- function(smat, maxintf){
  mat <- as.matrix(smat)
  mat <- mat + 1
  mat <- log(mat)
  mat <- mat/log(maxintf) 
  prepMatrix <- mat
}
