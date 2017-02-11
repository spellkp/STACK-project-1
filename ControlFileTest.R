###########################################################
# Building Control File                                   #
###########################################################

#Input Start Date and time
#Year Month Day Hour
StartYear <- 12
StartMonth <- 01
StartDay <- 01
StartHour <- 00

#Number of Starting Locations
NumOfStartLocs <- 3

#StartLocInfo1
lat1 <- 39.28684
lon1 <- -96.11821 
hght1 <- 174.96
vel1 <- 11.25 
area1 <- 47.17

#StartLocInfo2
lat2 <- 39.28681
lon2 <- -96.11721
hght2 <- 174.96
vel2 <- 11.57
area2 <- 47.17

#StartLocInfo3
lat3 <- 39.28681
lon3 <- -96.11618
hght3 <- 174.96 
vel3 <- 10.86
area3 <- 47.17

#Total Run Time (hr) Vertical Motion (mAGL) Top of Model (mAGL)
TotRunTime <- 24
VertMot <- 0
TopLvl <- 10000.0


#Feed in meteorology data: Number of met files and file paths
EDASpath <- "C:/hysplit4/working/"
EDASMonths <- c(1, 2, 3, 4, 5, 6)

#Selecting MET Data

MetData = NULL

EDASFileNames <- c("edas.jan12.001",
                   "edas.jan12.002",
                   "edas.feb12.001",
                   "edas.feb12.002",
                   "edas.mar12.001",
                   "edas.mar12.002",
                   "edas.apr12.001",
                   "edas.apr12.002",
                   "edas.may12.001",
                   "edas.may12.002",
                   "edas.jun12.001",
                   "edas.jun12.002"
)

for (i in 1:(2*max(EDASMonths))) {
  
  MetData[2*i-1] <- EDASpath
  MetData[2*i] <- EDASFileNames[i]
  
}

            

#Number of Pollutant Species
PolNum <- 1

#Pollutant Name (<5 characters)
NameTemp <- "CO2"

#Pollutant Emission Rate (mass/hr)
PolRat <- 560000

#Pollutant Duration (hr)
PolDur <- 24

cat(StartYear, " ", StartMonth, " ", StartDay, " ", StartHour, "\n", 
    NumOfStartLocs, "\n",
    lat1, " ", lon1, " ", hght1, " ", vel1, " ", area1, "\n",
    lat2, " ", lon2, " ", hght2, " ", vel2, " ", area2, "\n",
    lat3, " ", lon3, " ", hght3, " ", vel3, " ", area3, "\n",
    TotRunTime, "\n",
    VertMot, "\n",
    TopLvl, "\n",
    NumMetFiles, "\n",
    MetData, "\n",
    sep='', file = "datetest")
