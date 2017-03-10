#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 1            #1='Yes'; 0='No'#
FreeCores <- 1        #Number of cores available during computation

NumStacks <- 3        #Number of stacks at location

StackLoc <- rbind(#Insert c(LAT,LON); more can be added.
                  c(     ,     )
                  c(     ,     )
                  c(     ,     )
                  )

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
#####################################
#####################################


#####################################
##### Initialize Parallel Cores #####
#####################################
if (IfPar == 1) {
  cl <- detectCores()-FreeCores
  registerDoParallel(cl)
}
else {print("Serial Computation")}
#####################################

#####################################
##### Read in Data ##################
#####################################
Model1 <- read.csv(Model1)[2:6]
Model2 <- read.csv(Model2)[2:6]

Analysis1 = NULL

Metric1 = foreach(i=2:366, .combine = rbind) %dopar% {
  
  tempModel1 <- subset(Model1, Model1$Day == i)
  tempModel2 <- subset(Model2, Model2$Day == i)
  
}