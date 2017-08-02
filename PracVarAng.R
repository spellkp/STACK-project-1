## PRACTICING VARIANCE

library(geosphere)


for (i in 2:366) {

  plant <- c(-96.1172, 39.28682)
  
  lat <- c(39.24, 39.29, 39.14, 39.19)
  long <- c(-96.12, -96.07, -96.12, -96.02)
  conc <- c(1.9e-10, 2.5e-07, 7.1e-09, 1.9e-10)
  emit <- cbind(long, lat, conc)
  
  ex.angle <- bearing(plant, emit[,1:2])
  print(ex.angle)
  
  mean.angle <- sum(ex.angle*emit)/sum(emit)
  print(mean.angle)
  
  var.angle1 <- sqrt((sum(na.omit(emit)*(ex.angle - mean.angle)))^2)/sum(na.omit(emit))
  print(var.angle1)
  


  plant2 <- c(-96.1172, 39.28682)
  
  lat2 <- c(39.24, 39.29, 39.14, 39.19)
  long2 <- c(-96.12, -96.07, -96.12, -96.02)
  conc2 <- c(1.9e-10, 2.5e-07, 7.1e-09, 1.9e-10)
  emit2 <- cbind(long, lat, conc)
  
  ex.angle2 <- bearing(plant2, emit2[,1:2])
  print(ex.angle2)
  
  mean.angle2 <- sum(ex.angle2*emit2)/sum(emit2)
  print(mean.angle2)
  
  mean.angleJEC <- mean.angle2 - mean.angle
  print(mean.angleJEC)
  
}

####other things for code
plot(long, lat, xlim= c(-97, -94), ylim=c(39, 41))
points((-96.1172),(39.28682), col='red')

var.angle2 <- sqrt((sum(emit)*(sum(ex.angle - mean.angle)^2))/sum(emit))
print(var.angle2)