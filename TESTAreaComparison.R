#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 1            #1='Yes'; 0='No'#Multicore Capabilities
FreeCores <- 1        #Number of cores available during computation

StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  #c(39.28684, -96.11821),
  #c(39.28681, -96.11721),
  c(36.99781, -84.59239)
)

eGRIDLoc <- c(36.9981, -84.5919)

Model1 <- "JSC-A-2012"    #Name of dataset for Model 1; MUST BE CSV
Model2 <- "JSC-E-2012"    #Name of dataset for Model 2; MUST BE CSV

#####################################
##### Initialize All Libraries ######
#####################################
library(ggplot2)
library(dplyr)
library(geosphere)
library(foreach)
library(doParallel)
library(ggmap)
library(akima)
library(fields)
#####################################
#####################################

#####################################
##### Initialize Parallel Cores #####
#####################################
if (IfPar == 1)
{cl <- detectCores()-FreeCores
registerDoParallel(cl)
} else {print("Serial Computation")}
#####################################

#############################################
##### Area Calculation and Comparison   #####
#############################################

##### Model 1 (Single Stack) ########

Model1 <- read.csv(Model1)[2:6]
Model2 <- read.csv(Model2)[2:6]

SS <- 0.05

Areas = foreach(i = 2:366, .combine = rbind) %dopar% {
  tempModel1 <- subset(Model1, Model1$Day == i)
  tempModel2 <- subset(Model2, Model2$Day == i)
  
  Mod1 <- matrix(ncol = ((max(tempModel1$Lon)-min(tempModel1$Lon))/SS) + 1, nrow = ((max(tempModel1$Lat)-min(tempModel1$Lat))/SS) + 1)
  Mod2 <- matrix(ncol = ((max(tempModel2$Lon)-min(tempModel2$Lon))/SS) + 1, nrow = ((max(tempModel2$Lat)-min(tempModel2$Lat))/SS) + 1)
  
  tempModel1$Lon <- (tempModel1$Lon - min(tempModel1$Lon))/SS
  tempModel1$Lat <- (tempModel1$Lat - min(tempModel1$Lat))/SS

  tempModel2$Lon <- (tempModel2$Lon - min(tempModel2$Lon))/SS
  tempModel2$Lat <- (tempModel2$Lat - min(tempModel2$Lat))/SS


for (j in 1:nrow(Mod1)) {
  
  for (k in 1:ncol(Mod1)) {
    
    temp <- mean(tempModel1$Conc[tempModel1$Lon >= k & tempModel1$Lon >= k + 1 & tempModel1$Lat >= j & tempModel1$Lat >= j + 1])
    
    if(is.nan(temp)) {Mod1[j,k] <- 0} else {Mod1[j,k] <- 1}
    
  }
}



for (j in 1:nrow(Mod2)) {
  
  for (k in 1:ncol(Mod2)) {
    
    temp <- mean(tempModel2$Conc[tempModel2$Lon >= k & tempModel2$Lon >= k + 1 & tempModel2$Lat >= j & tempModel2$Lat >= j + 1])
    
    if(is.nan(temp)) {Mod2[j,k] <- 0} else {Mod2[j,k] <- 1}
    
  }
  
}

cbind(sum(Mod1), sum(Mod2))

  }

test <- as.data.frame((200*(Areas[,2] - Areas[,1])/(Areas[,2] + Areas[,1])))
colnames(test) <- "PercentDifference"
rownames(test) <- 2:366

MaxDay <- which.max(test$PercentDifference) + 1

PlotModel1 <- subset(Model1, Day == MaxDay)
PlotModel2 <- subset(Model2, Day == MaxDay)

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 05, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") 

##########

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 05, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") 


