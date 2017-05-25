library(NISTunits)
#takes in projcoords and central coords (1:2 numeric vectors) and returns the haversine distance (1:2 vector in km)using radius lamb (numeric scalar)
converttoNM <- function(rad, projcoords, centralcoords){
  lat1 <- NISTdegTOradian(projcoords[1])
  lat2 <- NISTdegTOradian(centralcoords[1])
  
  lon1 <- NISTdegTOradian(projcoords[2])
  lon2 <- NISTdegTOradian(centralcoords[2])
  
  dellat <- lat1 - lat2
  delLon <- lon1 - lon2
  
  a <- (sin(dellat/2)^2) + cos(lat1)*cos(lat2)*(sin(delLon/2)^2)
  c <- 2*atan2(sqrt(a),sqrt(1-a))
  d <- rad * c
  
  #kilometers to NM conversion
  converttoNM <- d * 0.539957
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


orlcenter <- c(28.5383,-81.3792)

testpoint <- c(28.45, -81.31)

orlrad <- findradius(orlcenter)

x <- converttoNM(orlrad, testpoint, orlcenter)



