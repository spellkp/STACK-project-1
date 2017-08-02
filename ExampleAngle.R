library(geosphere)
  
  plant <- c(-96.11721, 39.28681)
  
  lat <- 39.24+2*runif(10)
  long <- -96.12+2*runif(10)
  conc <- 1.4e-10+runif(10)/1000000
  emit <- cbind(long, lat, conc)
  
  ex.angle <- bearing(plant, emit[,1:2])
  print(ex.angle)
  
plot(long, lat, xlim= c(-97, -94), ylim=c(39, 41))
points((-96.11721),(39.28681), col='red')

mean.angle <- sum(ex.angle*emit)/sum(emit)
print(mean.angle)

var.angle <- sqrt((sum(emit)*(sum(ex.angle - mean.angle)^2))/sum(emit))
print(var.angle)

