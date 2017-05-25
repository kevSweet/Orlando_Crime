library(MeanShift)

#defines latandlong as columns of opd crimes offense type, lat, and long
latandlong <- subset(OPD_CrimesParsed_NoUnmapped, select = c("Case Offense Type", "Latitude", "Longitude"))

#narrows dataset to between 28.514520 and 28.561178 Latitude.  -81.401205 and-81.352166 Longitude
downtownlatandlong <- subset(latandlong, Latitude > 28.514520 & Latitude < 28.561178 & Longitude > -81.401205 & Longitude < -81.352166)

#generates a random matrix from latandlong dataset of size (2x10000). Matrix retains row indices
r.sample <- downtownlatandlong[sample(nrow(downtownlatandlong),size=5000,replace=FALSE),]

r.coords <- subset(r.sample, select = c("Latitude","Longitude"))

#performs mean shift clustering with bandwidth h

ms <- msClustering(t(r.coords),h=.005,multi.core = TRUE)

 plot(r.sample, col = ms$labels,xlab = "Latitude", ylab = "Longitude", main = "Mean Shift Clustering")

