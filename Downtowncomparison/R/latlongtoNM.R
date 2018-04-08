library(NISTunits)

#Takes in radius of the earth at a point and 2 [1:2] matrices for (lat,long). Projcoords is the point being projected and centralcoords is the coords that create the plane of projection
converttognomonic <- function(rad, projcoords, centralcoords){
  
  phin <-  NISTdegTOradian(projcoords[1])
  lambn <-  NISTdegTOradian(projcoords[2])
  
  phi <-  NISTdegTOradian(centralcoords[1])
  lamb <- NISTdegTOradian(centralcoords[2])
  
  x <- cos(phi)*sin(lamb-lambn)/(sin(phin)*sin(phi)+cos(phin)*cos(phi)*cos(lamb-lambn))
  y <- cos(phin)*sin(phi)-sin(phin)*cos(phi)*cos(lamb-lambn)/(sin(phin)*sin(phi)+cos(phin)*cos(phi)*cos(lamb-lambn))
  
  #0.539957 is conversion factor km -> naut miles
  dist <- sqrt(x^2 + y^2) * rad * 0.539957
  
  converttognomonic <- dist
  }
 
#passes the central coordinates
findradius <- function(centralcoords){
  #radius at equator (km)
  r1 <- 6378
  
  #radius at pole (km)
  r2 <- 6357
  
  radius <- sqrt(((r1*r1*cos(centralcoords[1]))^2) + (r2*r2*sin(centralcoords[1]))^2)/((r1*cos(centralcoords[1])^2) + (r2*sin(centralcoords[1])^2))
  
  return(radius)
  }





