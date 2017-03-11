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

MaxConcDist1 <- as.data.frame(MaxConcDist1)

Magic1 <- aggregate(. ~ Lat+Lon, data = MaxConcDist1, FUN = function(x) c(mn = mean(x)) )

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 10, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = Magic2, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(Magic1$Conc), min(Magic2$Conc)), 
                                max(max(Magic1$Conc), max(Magic2$Conc))), low = "yellow", high = "red")


MaxConcDist2 <- as.data.frame(MaxConcDist2)

Magic2 <- aggregate(. ~ Lat+Lon, data = MaxConcDist2, FUN = function(x) c(mn = mean(x)) )

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 10, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = Magic2, aes(x = Lon, y = Lat, fill = Conc)) +
          scale_fill_gradient(limits=c(min(min(Magic1$Conc), min(Magic2$Conc)), 
                                       max(max(Magic1$Conc), max(Magic2$Conc))), low = "yellow", high = "red")

##### Day-by-Day Comparison #####

PercentDifference = NULL
RModel1 = NULL
RModel2 = NULL

MaxConc = foreach(i=2:366, .combine = rbind) %dopar% {
  tempModel1 <- subset(Model1, Model1$Day == i)
  tempModel2 <- subset(Model2, Model2$Day == i)
  PercentDifference[i] <- ((max(tempModel2$Conc)-max(tempModel1$Conc))/mean(c(max(tempModel1$Conc), max(tempModel2$Conc))))*100
 
  LatModel1 <- tempModel1$Lat[tempModel1$Conc == max(tempModel1$Conc)]
  LonModel1 <- tempModel1$Lon[tempModel1$Conc == max(tempModel1$Conc)]
  RModel1[i] <- sqrt((Lat-Model1)^2+(Lon-Model1)^2)
  
  LatModel2 <- tempModel2$Lat[tempModel2$Conc == max(tempModel2$Conc)]
  LonModel2 <- tempModel2$Lon[tempModel2$Conc == max(tempModel2$Conc)]
  RModel2[i] <- sqrt((Lat-Model2)^2+(Lon-Model2)^2)
  
  cbind(RModel1[i], RModel2[i], PercentDifference[i])
  }

MaxConc <- as.data.frame(MaxConc)

colnames(MaxConc)[1] <- "Day"
colnames(MaxConc)[2] <- "PercentDifference"

ggplot(data = MaxConc, aes(x = Day, y = PercentDifference)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  xlab("Day of 2012") +
  ylab("Percent Difference (%)") +
  ggtitle("Jeffrey Energy Center (2012): \n Percent Difference in Simulated Maximum Concentration per Day") +
  theme_bw()












