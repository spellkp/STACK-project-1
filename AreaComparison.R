#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 0            #1='Yes'; 0='No'#Multicore Capabilities
FreeCores <- 1        #Number of cores available during computation

StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618)
)

eGRIDLoc <- c(39.2865, -96.1172)

Model1 <- "A-2012"    #Name of dataset for Model 1; MUST BE CSV
Model2 <- "E-2012"    #Name of dataset for Model 2; MUST BE CSV

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

DispArea1 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel1 <- subset(Model1, Model1$Day == i)
    
    s <- interp(x = tempModel1$Lon, y = tempModel1$Lat, z = tempModel1$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    sum(new_s, na.rm = TRUE)*12321
  }




Model2 <- read.csv(Model2)[2:6]

DispArea2 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel2 <- subset(Model2, Model2$Day == i)
    
    s <- interp(x = tempModel2$Lon, y = tempModel2$Lat, z = tempModel2$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    sum(new_s, na.rm = TRUE)*12321
  }

PercentDifference <- (DispArea2 - DispArea1)/mean(c(DispArea1, DispArea2))
