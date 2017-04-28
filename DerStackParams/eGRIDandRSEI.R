library(dplyr)
library(ggplot2)

WriteCSV <- 1   # 1 = yes, 0 = no

eGRID <- read.csv("eGRID2012_Data.csv", header = TRUE)
eGRID$NewFIPS <- 1000*eGRID$Plant.FIPS.state.code + eGRID$Plant.FIPS.county.code

RSEI <- read.csv("FacilityData2017.csv", header = TRUE)


#Determine a resolution using the for loop below.

NEWeGRID <- data.frame()

R <- 0.0005

NumOfPlants = NULL
NewDataSet <- data.frame()

for (i in 1:nrow(eGRID)) {
  
  subRSEI <- subset(RSEI, RSEI$FIPS == eGRID$NewFIPS[i])
  
  subRSEI$DistCheck <- sqrt((subRSEI$Latitude. - eGRID$Plant.latitude[i])^2 + (subRSEI$Longitude. - eGRID$Plant.longitude[i])^2)
  
  
FINALsubRSEI <- subset(subRSEI, subRSEI$DistCheck < R)
  
  if(nrow(FINALsubRSEI) != 0) {
    
    addline <- data.frame(eGRID$Plant.name[i], eGRID$eGRID2012.file.plant.sequence.number[i] + 1,
                                               subRSEI$Facility.Name.[which.min(FINALsubRSEI$DistCheck)],
                                               subRSEI$Facility.Number.[which.min(FINALsubRSEI$DistCheck)] + 1, 
                                               subRSEI$DistCheck[which.min(FINALsubRSEI$DistCheck)])
    
    NEWeGRID <- rbind(NEWeGRID, addline)
    
  } else {}
  
}

names(NEWeGRID) <- c("eGRID Plant Name", "eGRID Number", "RSEI Plant Name", "RSEI Number", "Distance")


for(j in 1:nrow(NEWeGRID)) {
  
  Combined_Data <- cbind(eGRID[eGRID$eGRID2012.file.plant.sequence.number == NEWeGRID$`eGRID Number`[j],],
                         RSEI$Stack.Height.[RSEI$Facility.Number. == NEWeGRID$`RSEI Number`[j]],
                         RSEI$Stack.Diameter.[RSEI$Facility.Number. == NEWeGRID$`RSEI Number`[j]],
                         RSEI$Stack.Velocity.[RSEI$Facility.Number. == NEWeGRID$`RSEI Number`[j]])
  
  NewDataSet <- rbind(NewDataSet, Combined_Data)
  
}
  
names(NewDataSet)[169] <- "Height"
names(NewDataSet)[170] <- "Diameter"
names(NewDataSet)[171] <- "Velocity"
  
if(WriteCSV = 1) {
  
  write.csv(NewDataSet, "NewDataSet.csv")
  
}  else {}
  
ggplot(data = NewDataSet, aes(x = Plant.annual.SO2.combustion.output.emission.rate..lb.MWh., y = Height)) +
  geom_point() +
  geom_smooth() +
  theme_bw()
  


### use this for the Plants vs R graph ##3

#for (j in 1:40) {

#  R <- j*0.00005
  
#    for (i in 1:nrow(eGRID)) {
  
#        subRSEI <- subset(RSEI, RSEI$FIPS == eGRID$NewFIPS[i])
  
#        subRSEI$DistCheck <- sqrt((subRSEI$Latitude. - eGRID$Plant.latitude[i])^2 + (subRSEI$Longitude. - eGRID$Plant.longitude[i])^2)
    
#        subRSEI <- subset(subRSEI, subRSEI$DistCheck < R)
    
#            if(nrow(subRSEI) != 0) {
         
#                addline <- data.frame(eGRID$Plant.name[i], subRSEI$Facility.Name.[which.min(subRSEI$DistCheck)], subRSEI$DistCheck[which.min(subRSEI$DistCheck)])
          
#                NEWeGRID <- rbind(NEWeGRID, addline)
        
#            } else {}
    
#      }

#    RadPlot[j,1] <- R
#    RadPlot[j,2] <- nrow(NEWeGRID)
    
#    NEWeGRID <- data.frame()
#}
