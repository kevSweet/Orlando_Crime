library(MeanShift)

#defines latandlong as columns of opd crimes offense type, lat, and long

latandlong <- OPD_CrimesParsed_NoUnmapped[,c("Case Offense Type", "Latitude", "Longitude")]



#generates a random dataframe from latandlong dataset of size "size". Matrix retains row indices
r.sample <- latandlong[sample(nrow(latandlong),size=5000,replace=FALSE),]

r.samplecoord <- cbind(r.sample[,2], r.sample[,3])


#performs mean shift clustering with bandwidth h

ms <- msClustering(t(r.samplecoord),h=.02,multi.core = TRUE)

plot(r.sample, col = ms$labels,xlab = "Latitude", ylab = "Longitude", main = "Mean Shift Clustering")

