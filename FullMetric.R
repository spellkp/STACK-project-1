#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 0            #1='Yes'; 0='No'#Multicore Capabilities
FreeCores <- 1        #Number of cores available during computation

  StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
    c(39.28684, -96.11821),
    c(39.28681, -96.11721),
    c(39.28681, -96.11618))
  
eGRIDLoc <- c(39.2865, -96.1172)

Model1 <- "JEC-B-2012"    #Name of dataset for Model 1; MUST BE CSV
Model2 <- "JEC-E-2012"    #Name of dataset for Model 2; MUST BE CSV

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
##### Analysis of Maximum Concentration #####
#############################################

Model1 <- read.csv(Model1)[2:6]
Model2 <- read.csv(Model2)[2:6]


Comp1PercentDifference = NULL
Comp2PercentDifference = NULL
Comp3PercentDifference = NULL
                                    
  for (i in 2:366) {                
    #Max Concentration Comparison
    tempModel1 <- subset(Model1, Model1$Day == i)
    tempModel2 <- subset(Model2, Model2$Day == i)
    
    Comp1PercentDifference[i] <- (max(tempModel2$Conc) - max(tempModel1$Conc))/mean(c(max(tempModel1$Conc), max(tempModel2$Conc)))
  }
   

for (i in 2:366) { 
    #Weighted Center Comparison
    tempModel1 <- subset(Model1, Model1$Day == i)
    tempModel2 <- subset(Model2, Model2$Day == i)
  
    AvgLat1 <- sum((tempModel1$Lat-mean(StackLoc[,1]))*tempModel1$Conc)/sum(tempModel1$Conc)
    AvgLon1 <- sum((tempModel1$Lon-mean(StackLoc[,2]))*tempModel1$Conc)/sum(tempModel1$Conc)
    CentModel1 <- sqrt(AvgLat1^2 + AvgLon1^2)
    
    AvgLat2 <- sum((tempModel2$Lat-mean(StackLoc[,1]))*tempModel2$Conc)/sum(tempModel2$Conc)
    AvgLon2 <- sum((tempModel2$Lon-mean(StackLoc[,2]))*tempModel2$Conc)/sum(tempModel2$Conc)
    CentModel2 <- sqrt(AvgLat2^2 + AvgLon2^2)
    
    Comp2PercentDifference[i] <- (CentModel2 - CentModel1)/mean(c(CentModel2, CentModel1))
}
 

##### This is stupid... Get it together R programmers! #####


for (i in 2:366) {   
    #Area Comparison
    tempModel1 <- subset(Model1, Model1$Day == i)
    tempModel2 <- subset(Model2, Model2$Day == i)
  
    s <- interp(x = tempModel1$Lon, y = tempModel1$Lat, z = tempModel1$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    AreaModel1 <- sum(new_s, na.rm = TRUE)*12321
    
    s <- interp(x = tempModel2$Lon, y = tempModel2$Lat, z = tempModel2$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    AreaModel2 <- sum(new_s, na.rm = TRUE)*12321
    
    Comp3PercentDifference[i] <- (AreaModel2 - AreaModel1)/mean(c(AreaModel1, AreaModel2))
    
  }


PercentDifferenceData <- data.frame(Comp1PercentDifference, Comp2PercentDifference, Comp3PercentDifference, 
                                    sqrt(Comp1PercentDifference^2 + Comp2PercentDifference^2 + Comp3PercentDifference^2))

