##Files will not be read into R

library(geosphere)

Model1 <- read.csv("JEC-A-2012.csv")
Model2 <- read.csv("JEC-E-2012.csv")

for (i in 2:366) {
  
  DayModel1 <- subset(Model1, Day == i)     #This will create a dataframe (DayModel1) that only has Day "i" in it.
  DayModel2 <- subset(Model2, Day == i)     #This will create a dataframe (DayModel2) that only has Day "i" in it.
  
  a <- c(39.28684, -96.11821) #lat, long of plant
  b <- c(39.28681, -96.11721)
  c <- c(39.28681, -96.11618)  
  plant <- c(a, b, c)
  
  conc1 <- Model1[,4] #lat
  conc2 <- Model2[,5] #long
  emit <- c(conc1, conc2)
  
  anumber <- bearing(plant, emit)
  
}

mean.angle <- sum(angle*emit)/sum(emit)