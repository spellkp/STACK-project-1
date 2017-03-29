#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 0           #1='Yes'; 0='No'#Multicore Capabilities
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

PercentDifference = NULL

for (i in 2:366) {
  
  tempModel1 <- subset(Model1, Day == i)
  tempModel2 <- subset(Model2, Day == i)
  
  RadModel1 <- data.frame(sqrt((tempModel1$Lat - mean(StackLoc[,1]))^2 + (tempModel1$Lon - mean(StackLoc[,2]))^2), tempModel1$Conc)
  RadModel2 <- data.frame(sqrt((tempModel2$Lat - mean(StackLoc[,1]))^2 + (tempModel2$Lon - mean(StackLoc[,2]))^2), tempModel2$Conc)
  names(RadModel1) <- c('Radius', 'Concentration')
  names(RadModel2) <- c('Radius', 'Concentration')
  
  binwidth <- max(max(RadModel1$Radius), max(RadModel2$Radius))/100
  
  ConcProf1 = NULL
  ConcProf2 = NULL
  
      for (j in 0:99) {
    
        ConcProf1[j] <- mean(RadModel1$Conc[RadModel1$Radius > j*binwidth & RadModel1$Radius <= (j+1)*binwidth])
        ConcProf2[j] <- mean(RadModel2$Conc[RadModel2$Radius > j*binwidth & RadModel2$Radius <= (j+1)*binwidth])
      }
  
PercentDifference[i] <- mean(na.omit(200*(ConcProf2-ConcProf1)/(ConcProf2+ConcProf1)))

}

MaxDay <- which.max(PercentDifference)

tempModel1 <- subset(Model1, Day == MaxDay)
tempModel2 <- subset(Model2, Day == MaxDay)

RadModel1 <- data.frame(sqrt((tempModel1$Lat - mean(StackLoc[,1]))^2 + (tempModel1$Lon - mean(StackLoc[,2]))^2), tempModel1$Conc)
RadModel2 <- data.frame(sqrt((tempModel2$Lat - mean(StackLoc[,1]))^2 + (tempModel2$Lon - mean(StackLoc[,2]))^2), tempModel2$Conc)
names(RadModel1) <- c('Radius', 'Concentration')
names(RadModel2) <- c('Radius', 'Concentration')

binwidth <- max(max(RadModel1$Radius), max(RadModel2$Radius))/100

ConcProf1 = NULL
ConcProf2 = NULL

for (j in 0:99) {
  
  ConcProf1[j] <- mean(RadModel1$Conc[RadModel1$Radius > j*binwidth & RadModel1$Radius <= (j+1)*binwidth])
  ConcProf2[j] <- mean(RadModel2$Conc[RadModel2$Radius > j*binwidth & RadModel2$Radius <= (j+1)*binwidth])
}

plot(ConcProf1)
points(ConcProf2)