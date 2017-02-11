###########################################################
# Building Control File                                   #
###########################################################

#Input Start Date and time
#Year Month Day Hour
StartYear <- NULL
StartMonth <- NULL
StartDay <- NULL
StartHour <- NULL

#Number of Starting Locations
#Enter integer number for number of starting locations.
#Enter lat(dec), lon(dec), height(mAGL), exhaust velocity(m/s), exhaust area(m^2)

NumOfStartLocs <- 3
StartLocInfo1 <- "39.28684 -96.11821 174.96 11.25 47.17"
StartLocInfo2 <- "39.28681 -96.11721 174.96 11.57 47.17"
StartLocInfo3 <- "39.28681 -96.11618 174.96 10.86 47.17"

#Total Run Time (hr) Vertical Motion (mAGL) Top of Model (mAGL)

TotRunTime <- "24"
VertMot <- "0"
TopLvl <- "10000.0"


#Feed in meteorology data: Number of met files and file paths

NumMetFiles <- 12
MetData <-  c('C:/hysplit4/working/','edas.jun12.002',
              'C:/hysplit4/working/','edas.mar12.001',
              'C:/hysplit4/working/','edas.mar12.002',
              'C:/hysplit4/working/','edas.may12.001',
              'C:/hysplit4/working/','edas.may12.002',
              'C:/hysplit4/working/','edas.apr12.001',
              'C:/hysplit4/working/','edas.apr12.002',
              'C:/hysplit4/working/','edas.feb12.001',
              'C:/hysplit4/working/','edas.feb12.002',
              'C:/hysplit4/working/','edas.jan12.001',
              'C:/hysplit4/working/','edas.jan12.002',
              'C:/hysplit4/working/','edas.jun12.001')
            

#Number of Pollutant Species
PolNum <- 1

#Pollutant Name (<5 characters)
NameTemp <- "CO2"

#Pollutant Emission Rate (mass/hr)
PolRat <- 560000

#Pollutant Duration (hr)
PolDur <- "24"

ControlFile <- c(BeginDate, NumOfStartLocs, StartLocInfo1, StartLocInfo2, StartLocInfo3, SimDur, BotLvl, TopLvl,
                 NumMetFiles, MetData, PolNum, NameTemp, PolRat, PolDur)
