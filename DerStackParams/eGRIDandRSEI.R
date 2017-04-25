library(dplyr)
library(ggplot2)

eGRID <- read.csv("eGRID2012_Data.csv")
eGRID$NewFIPS <- 1000*eGRID$Plant.FIPS.state.code + eGRID$Plant.FIPS.county.code

RSEI <- read.csv("FacilityData2017.csv")


#Determine a resolution using the for loop below.

NEWeGRID <- data.frame()

R <- 0.001

NumOfPlants = NULL
RadPlot <- matrix(0, nrow = 40, ncol = 2)

for (j in 1:40) {

  R <- j*0.00005
  
    for (i in 1:nrow(eGRID)) {
  
        subRSEI <- subset(RSEI, RSEI$FIPS == eGRID$NewFIPS[i])
  
        subRSEI$DistCheck <- sqrt((subRSEI$Latitude. - eGRID$Plant.latitude[i])^2 + (subRSEI$Longitude. - eGRID$Plant.longitude[i])^2)
    
        subRSEI <- subset(subRSEI, subRSEI$DistCheck < R)
    
            if(nrow(subRSEI) != 0) {
          
                addline <- data.frame(eGRID$Plant.name[i], subRSEI$Facility.Name.[which.min(subRSEI$DistCheck)], subRSEI$DistCheck[which.min(subRSEI$DistCheck)])
          
                NEWeGRID <- rbind(NEWeGRID, addline)
        
            } else {}
    
      }

    RadPlot[j,1] <- R
    RadPlot[j,2] <- nrow(NEWeGRID)
    
    NEWeGRID <- data.frame()
}
