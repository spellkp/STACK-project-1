library(ggplot2)
library(dplyr)

#Read in models to compare
ModelA <- read.csv("A-2012")
ModelA <- ModelA[2:6]
ModelE <- read.csv("E-2012")
ModelE <- ModelE[2:6]

#Comparing maximum concentration
Day = NULL
PercentDifference = NULL

for (i in 2:366) {

  Day[i] <- i
  tempModelA <- subset(ModelA, ModelA$Day == i)
  tempModelE <- subset(ModelE, ModelE$Day == i)
  PercentDifference[i] <- ((max(tempModelE$Conc)-max(tempModelA$Conc))/mean(c(max(tempModelA$Conc), max(tempModelE$Conc))))*100

}

MaxConc <- data.frame(Day, PercentDifference)

ggplot(data = MaxConc, aes(x = Day, y = PercentDifference)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  xlab("Day of 2012") +
  ylab("Percent Difference (%)") +
  ggtitle("Jeffrey Energy Center (2012): \n Percent Difference in Simulated Maximum Concentration per Day") +
  theme_bw()

#Comparing location of COM (Unweighted)
Day2 = NULL
PercentDifference2 = NULL

for (i in 2:366) {
  
  Day2[i] <- i
  tempModelA <- subset(ModelA, ModelA$Day == i)
  tempModelE <- subset(ModelE, ModelE$Day == i)
  
  LatA <- mean(tempModelA$Lat)
  LonA <- mean(tempModelA$Lon)
  LatE <- mean(tempModelE$Lat)
  LonE <- mean(tempModelE$Lon)
  
  PercentDifference2[i] <- 200*sqrt(((LatA-LatE)/(LatA+LatE))^2 + ((LonA-LonE)/(LonA+LonE))^2)
  
}

CenterConc <- data.frame(Day2, PercentDifference2)

ggplot(data = CenterConc, aes(x = Day2, y = PercentDifference2)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  xlab("Day of 2012") +
  ylab("Percent Difference (%)") +
  ggtitle("Jeffrey Energy Center (2012): \n Percent Difference in Center of Dispersion (Unweighted) per Day") +
  theme_bw()

#Comparing location of COM (Weighted)
Day3 = NULL
PercentDifference3 = NULL

for (i in 2:366) {
  
  Day3[i] <- i
  tempModelA <- subset(ModelA, ModelA$Day == i)
  tempModelE <- subset(ModelE, ModelE$Day == i)
  
  LatA_bar <- sum(tempModelA$Lat*tempModelA$Conc)/sum(tempModelA$Conc)
  LonA_bar <- sum(tempModelA$Lon*tempModelA$Conc)/sum(tempModelA$Conc)
  LatE_bar <- sum(tempModelE$Lat*tempModelE$Conc)/sum(tempModelE$Conc)
  LonE_bar <- sum(tempModelE$Lat*tempModelE$Conc)/sum(tempModelE$Conc)
  
  PercentDifference3[i] <- 200*sqrt(((LatA_bar-LatE)/(LatA_bar+LatE))^2 + ((LonA-LonE)/(LonA+LonE))^2)
  
}

CenterConc2 <- data.frame(Day3, PercentDifference3)

ggplot(data = CenterConc2, aes(x = Day2, y = PercentDifference3)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  xlab("Day of 2012") +
  ylab("Percent Difference (%)") +
  ggtitle("Jeffrey Energy Center (2012): \n Percent Difference in Center of Dispersion (Weighted) per Day") +
  theme_bw()

#Comparing area of dispersion
for (i in 2:366) {

  tempModelA <- subset(ModelA, ModelA$Day == i)
  tempModelA <- as.data.frame(tempModelA[order(tempModelA$Lon, tempModelA$Lat),])

  tempModelE <- subset(ModelE, ModelE$Day == i)
  tempModelE <- as.data.frame(tempModelE[order(tempModelE$Lon, tempModelE$Lat),])
  
#Begin finding the area of the dispersion for Day[i], ModelA
deltaLat1 <- (max(tempModelA$Lat)-min(tempModelA$Lat))/0.01
deltaLon1 <- (max(tempModelA$Lon)-min(tempModelA$Lon))/0.01
store1 = NULL
q1 = 1
temp1 = 1

  for (q in 0:deltaLat1) {  
   
   for (j in 0:deltaLon1) {
  
    test1 <- which(ModelA$Lon > min(ModelA$Lon)+0.01*i & ModelA$Lon < min(ModelA$Lon) + 0.01*(i+1)
                & ModelA$Lat > min(ModelA$Lat)+0.01*j & ModelA$Lat < min(ModelA$Lat) + 0.01*(j+1))
     
        if (length(test1) == 0) {}
        else {store1[q1] <- 1
             q1=q1+1}
  
    
        if (round(j/deltaLon1, 2)*100 == temp1) {}
        else {temp1 <- 100*round((j/deltaLon1), 2)
   
          print(paste("Model A", "     ", "Year:", round(i/366, 2)*100, "%", "     ","Latitude Complete:", round(q/deltaLat1, 2)*100, "%", "     ",
                "Longitude Complete:", temp1, "%", sep = ""))
           }

      }
    
  }
  
#Begin finding the area of the dispersion for Day[i], ModelE
    deltaLat2 <- (max(tempModelE$Lat)-min(tempModelE$Lat))/0.01
    deltaLon2 <- (max(tempModelE$Lon)-min(tempModelE$Lon))/0.01
    store2 = NULL
    q2 = 1
    temp2 = 1
    
    for (q in 0:deltaLat2) {  
      
      for (j in 0:deltaLon2) {
        
        test2 <- which(ModelE$Lon > min(ModelE$Lon)+0.01*i & ModelE$Lon < min(ModelE$Lon) + 0.01*(i+1)
                       & ModelE$Lat > min(ModelE$Lat)+0.01*j & ModelE$Lat < min(ModelE$Lat) + 0.01*(j+1))
        
        if (length(test2) == 0) {}
        else {store2[q2] <- 1
        q2=q2+1}
        
        
        if (round(j/deltaLon2, 2)*100 == temp2) {}
        else {temp2 <- 100*round((j/deltaLon2), 2)
        
        print(paste("Model B", "     ", "Year:", round(i/366, 2)*100, "%", "     ","Latitude Complete:", round(q/deltaLat2, 2)*100, "%", "     ",
                    "Longitude Complete:", temp2, "%", sep = ""))
        }
        
      }
  

  }

  }
