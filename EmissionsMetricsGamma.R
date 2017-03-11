#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 1            #1='Yes'; 0='No'#
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
##### Analysis of Maximum Concentration #####
#############################################

##### Model 1 (Single Stack) ########

Model1 <- read.csv(Model1)[2:6]

MaxConcDist1 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel1 <- subset(Model1, Model1$Day == i)
    
    Index <- which(tempModel1$Conc == max(tempModel1$Conc))
    Lat <- tempModel1$Lat[Index]
    Lon <- tempModel1$Lon[Index]
    Conc <- tempModel1$Conc[Index]
    cbind(Lat, Lon, Conc)
    
  }

MaxConcDist1 <- as.data.frame(MaxConcDist1)

Magic1 <- aggregate(. ~ Lat+Lon, data = MaxConcDist1, FUN = function(x) c(mn = mean(x)) )

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 09, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = Magic1, aes(x = Lon, y = Lat, fill = Conc)) +
                scale_colour_gradientn(colours = rev(rainbow(4)))


##### Model 2 (Full Model) ##########

Model2 <- read.csv(Model2)[2:6]

MaxConcDist2 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel2 <- subset(Model2, Model2$Day == i)
    
    Index <- which(tempModel2$Conc == max(tempModel2$Conc))
    Lat <- tempModel2$Lat[Index]
    Lon <- tempModel2$Lon[Index]
    Conc <- tempModel2$Conc[Index]
    cbind(Lat, Lon, Conc)
    
  }


MaxConcDist2 <- as.data.frame(MaxConcDist2)

Magic2 <- aggregate(. ~ Lat+Lon, data = MaxConcDist2, FUN = function(x) c(mn = mean(x)) )

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 09, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = Magic2, aes(x = Lon, y = Lat, fill = Conc)) +
          scale_colour_gradientn(colours = rev(rainbow(4)))