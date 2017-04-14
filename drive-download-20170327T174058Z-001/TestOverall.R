### Overall Percent Difference Metric
Model1 <- "JEC-A-2012"
Model2 <- "JEC-E-2012"

StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618)
)

eGRIDLoc <- c(39.2865, -96.1172)

###################################################################
############ NO USER INPUT REQUIRED BELOW #########################
###################################################################

Model1 <- read.csv(Model1)
Model2 <- read.csv(Model2)

for(i in 2:366) {
  
  #Subset data by day
  DayModel1 <- subset(Model1, Model1$Day == i)
  DayModel2 <- subset(Model2, Model2$Day == i)
  
  #Normalize position to be centered above the power plant
  DayModel1$Lat <- DayModel1$Lat - mean(StackLoc[,1])
  DayModel1$Lon <- DayModel1$Lon - mean(StackLoc[,2])
  
  DayModel2$Lat <- DayModel2$Lat - mean(StackLoc[,1])
  DayModel2$Lon <- DayModel2$Lon - mean(StackLoc[,2])
  
  #Set up the matrices to be compared
  Matrix1 = matrix(0, nrow = max((max(DayModel1$Lat) - min(DayModel1$Lat))/0.1,
                              (max(DayModel2$Lat) - min(DayModel2$Lat))/0.1) + 1,
                   ncol = max((max(DayModel1$Lon) - min(DayModel1$Lon))/0.1,
                              (max(DayModel2$Lon) - min(DayModel2$Lon))/0.1) + 1)
  Matrix2 <- Matrix1
  
  
    #Construct the matrix for Model1 by finding the number of elements necessary at 0.1 degree resolution
    for (k in 1:ncol(Matrix1)) {
  
        for (j in 1:nrow(Matrix1)) {
          
          Matrix1[j,k] <- na.omit(mean(DayModel1$Conc[DayModel1$Lat >= (j-1)*min(DayModel1$Lat) & 
                                                      DayModel1$Lat < j*min(DayModel1$Lat) &
                                                      DayModel1$Lon >= (k-1)*min(DayModel1$Lon) & 
                                                      DayModel1$Lon < k*min(DayModel1$Lon)]))
                }
        }
  


    #Construct the matrix for Model2
    for (k in 1:ncol(Matrix2)) {
  
      for (j in 1:nrow(Matrix2)) {
               
                Matrix2[j,k] <- na.omit(mean(DayModel2$Conc[DayModel2$Lat >= (j-1)*min(DayModel2$Lat) & 
                                                            DayModel2$Lat < j*min(DayModel2$Lat) &
                                                            DayModel2$Lon >= (k-1)*min(DayModel2$Lon) & 
                                                            DayModel2$Lon < k*min(DayModel2$Lon)]))
                
              }
         }
  

    #Finding the difference between the two matrices
    Differences[i] <- mean(Matrix2 - Matrix1)

}
