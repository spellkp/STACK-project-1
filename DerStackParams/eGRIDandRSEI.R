library(dplyr)
library(ggplot2)

eGRID <- read.csv("eGRID2012_Data.csv")
RSEI <- read.csv("FacilityData2017.csv")

eGRID$NewFIPS <-1000*eGRID$Plant.FIPS.state.code + eGRID$Plant.FIPS.county.code

UniqEGRID <- unique(eGRID$NewFIPS)
eGRID2 <- sapply(eGRID, "[", UniqEGRID)

UniqRSEI <- unique(RSEI$FIPS)


CommonFIPS <- intersect(UniqEGRID, UniqRSEI)

