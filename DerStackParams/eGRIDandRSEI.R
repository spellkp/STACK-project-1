library(dplyr)

eGRID <- read.csv("eGRID2012_Data.csv")
eGRIDGen <- read.csv("eGRID2012_GenData.csv")
RSEI <- read.csv("FacilityData2017.csv")

eGRID$NewFIPS <- 1000*eGRID$Plant.FIPS.state.code + eGRID$Plant.FIPS.county.code

CommonFIPS <- intersect(eGRID$NewFIPS, RSEI$FIPS)

MODeGRID <- as.data.frame(sapply(eGRID, "[", CommonFIPS))
MODeGRID <- MODeGRID[order(MODeGRID$NewFIPS),]

MODRSEI <- as.data.frame(sapply(RSEI, "[", CommonFIPS))
MODRSEI <- MODRSEI[order(MODRSEI$FIPS),]

unique <- subset(MODRSEI, duplicated(MODRSEI$FIPS) != TRUE)



CommonFIPS2 <- intersect(eGRID$NewFIPS, unique$FIPS)

MODeGRID2 <- as.data.frame(sapply(eGRID, "[", CommonFIPS2))
MODeGRID2 <- MODeGRID2[order(MODeGRID2$NewFIPS),]

MODRSEI2 <- as.data.frame(sapply(RSEI, "[", CommonFIPS2))
MODRSEI2 <- MODRSEI2[order(MODRSEI2$FIPS),]
