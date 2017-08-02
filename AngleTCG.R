library(geosphere)

Model1 <- read.csv("TCG-A-2012-20km") 
Model2 <- read.csv("TCG-E-2012-20km") ##all parameters included

for (i in 2:366) {
  ## TCG-A
  DayModel1 <- subset(Model1, Day == i)     #This will create a dataframe (DayModel1) that only has Day "i" in it.
  
  plant <- c(-122.85815, 46.75544)
  
  lat <- DayModel1[,4]
  long <- DayModel1[,5]
  conc <- DayModel1[,6]
  emit <- cbind(long, lat, conc)
  
  ex.angle <- bearing(plant, emit[,1:2])
  print(ex.angle)
  
  mean.angle <- sum(ex.angle*emit)/sum(emit)
  print(mean.angle)
  
  ## TCG-E
  
  DayModel2 <- subset(Model2, Day == i)     #This will create a dataframe (DayModel2) that only has Day "i" in it.
  
  plant2 <- c(-122.85815, 46.75544)
  
  lat2 <- DayModel2[,4]
  long2 <- DayModel2[,5]
  conc2 <- DayModel2[,6]
  emit2 <- cbind(long, lat, conc)
  
  ex.angle2 <- bearing(plant2, emit2[,1:2])
  print(ex.angle2)
  
  mean.angle2 <- sum(ex.angle2*emit2)/sum(emit2)
  print(mean.angle2)
  
  ### Difference of E - A
  mean.angleJEC <- mean.angle2 - mean.angle
  print(mean.angleJEC)
  
  var.angleJEC <- var.angle2 - var.angle1
  print(var.angleJEC)
  
}