library(dplyr)
library(ggplot2)

WriteCSV <- 1   # 1 = yes, 0 = no

eGRID <- read.csv("eGRID2012_Data.csv", header = TRUE)[,c(1, 2, 3, 18, 19, 21, 22, 23, 25, 26, 32, 33, 40, 42, 44, 46, 47, 48, 49,
                                                          51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 65, 66, 67, 68, 69,
                                                          70, 72, 73, 74, 75, 76, 77, 79, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
                                                          91, 92, 93)]

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
  
names(NewDataSet) <- c("Plant Number", "State", "Plant Name", "FIPS State Code", "FIPS County Code", "Latitude", "Longitude", "Centroid", "Generators", "Combustion Status",
                       "Capacity Factor", "Nameplate Capacity", "Annual Heat Input", "Annual Net Generation", "Annual NOx Emissions",  "Annual SO2 Emissions", "Annual CO2 Emissions",
                       "Annual CH4 Emissions", "Annual N20 Emissions", "Annual Hg Emissions", "Annual NOx Output Emission Rate", "Ozone Season NOx Output Emission Rate",
                       "Annual SO2 Output Emission Rate", "Annual CO2 Output Emission Rate", "Annual CH4 Output Emission Rate", "Annual N2O Output Emission Rate",
                       "Annual CO2 Equivalent Emission Rate", "Annual Hg Output Emission Rate", "Annual NOx Output Emission Rate", "Ozone Season NOx Input Emission Rate",
                       "Annual SO2 Input Emission Rate", "Annual CO2 Input Emission Rate", "Annual NOx Combustion Output Emission Rate", "Ozone Season NOx Combustion Output Emission Rate",
                       "Annual SO2 Combustion Output Emission Rate", "Annual CO2 Combustion Output Emission Rate", "Annual CH4 Combustion Output Emission Rate",
                       "Annual N2O Combustion Output Emission Rate", "Unadjusted Annual NOx Emissions", "Unadjusted Ozone Season NOx Emissions", "Unadjusted Annual SO2 Emissions",
                       "Unadjusted Annual CO2 Emissions", "Unadjusted Annual CH4 Emissions", "Unadjusted Annual N2O Emissions", "Unadjusted Annual Heat Input", "Nomial Heat Rate",
                       "Annual Coal Net Generation", "Annual Oil Net Generation", "Annual Gas Net Generation", "Annual Nuclear Net Generation", "Annual Hydro Net Generation",
                       "Annual Biomass Net Generation", "Annual Wind Net Generation", "Annual Solar Net Generation", "Annual Geothermal Net Generation", "Annual Other Fossil Net Generation",
                       "Annual Unknown Net Generation", "Plant Annual Nonrenewable Net Generation", "NewFIPS", "Height", "Diameter", "Velocity")
  
if(WriteCSV == 1) {
  
  write.csv(NewDataSet, "NewDataSet.csv")
  
}  else {}
  






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
