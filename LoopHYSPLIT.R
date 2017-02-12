############################################################
# Building the Control File                               ##
############################################################

####################################
### A - EPA data used (only)   #####
### B - 0m Stack Height        #####
### C - 0m/s Emissions         #####
### D - 0m^2 area              #####
### E - Full Model             #####
####################################
ModelType <- "E"               #####
####################################

for (j in 1:31) {
  
####################################
### Input Start Date and time  #####
### Year Month Day Hour        #####
####################################
Start <- c(12, 01, j, 00)     #####
####################################

####################################
### Number of Starting Locations ###
####################################
NumOfStartLocs <- 3              ###
####################################

####################################
### StartLocInfo1 ##################
### lat lon hght velocity area #####
####################################
StartLocInfo1 <- c(39.28684, -96.11821, 174.96, 11.25, 47.17)
####################################

####################################
### StartLocInfo2 ##################
####################################
StartLocInfo2 <- c(39.28681, -96.11721, 174.96, 11.57, 47.17)
####################################

####################################
### StartLocInfo3 ##################
####################################
StartLocInfo3 <- c(39.28681, -96.11618, 174.96, 10.86, 47.17)
####################################

####################################
### Total Run Time (hr)        #####
### Vertical Motion (mAGL)     #####
### Top of Model (mAGL)        #####
####################################
TotRunTime <- 24               #####
VertMot <- 0                   #####
TopLvl <- 10000.0              #####
####################################

####################################
### Number of Pollutant Species    #
### (Number of grids/dep. same)    #
### Pollutant Name (<5 char)       #
### Pollut. Em. Rate (mass/hr)     #
### Pollutant Duration (hr)        #
### Release Start (YY MM DD HH MM) #
####################################
PolNum <- 1                        #
NameTemp <- "CO2"                  #
PolRat <- 560000                   #
PolDur <- 24                       #
RelStart <- c(12, 01, j, 00, 00)  #
####################################

####################################
### Display Grid               #####
### Center (lat lon)           #####
### Spacing (lat lon)          #####
### Span (lat lon)             #####
### Output directory           #####
####################################
CenterLatLon <- c(39.28681, -96.11721)
Spacing <- c(0.05, 0.05)
Span <- c(80.0, 80.0)
OutputDir <- "./"
OutputName <- paste(ModelType, Start[1], Start[2], Start[3], Start[4], sep = "-", collapse = NULL)
#####################################


#####################################
### Vertical Levels             #####
### Top of Model                #####
#####################################
Layers <- c(1, 10000)
#####################################


#####################################
### Start/Stop Sampling         #####
### YY MM DD HH MM              #####
### Method (XX HH MM)           #####
#####################################
SampleStart <- c(12, 01, j, 00, 00)
SampleStop <- c(12, (if (j <= 30) {01} else {02}), (if (j <= 30) {j+1} else {1}), 00, 00)
Method <- c(00, 24, 00)
#####################################


#####################################
### Chemical Parameters         #####
#####################################
### Particle Diameter (um)      #####
### Density (g/cc)              #####
### Shape                       #####
### Velocity (m/s)              #####
### Mol Wgt (g)                 #####
### A-Ratio                     #####
### D-Ratio                     #####
### Henry                       #####
### Henry's Constant (M/a)      #####
### In-cloud (l/l)              #####
### Below-cloud (1/s)           #####
### Radioactive decay (days)    #####
### Pollutant Resus Factor (1/m)#####
#####################################
ChemParams1 <- c(0.000257, 0.00197, 1.0)
ChemParams2 <- c(0.0, 0.0, 0.0, 0.0, 0.0)
ChemParams3 <- c(101.325, 0.0, 0.0)
ChemParams4 <- c(0.0, 0.0)

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

cat(paste(Start, collapse = " "), "\n",
    NumOfStartLocs, "\n",
    paste(StartLocInfo1, collapse = " "), "\n",
    paste(StartLocInfo2, collapse = " "), "\n",
    paste(StartLocInfo3, collapse = " "), "\n",
    TotRunTime, "\n",
    VertMot, "\n",
    TopLvl, "\n",
    2*max(EDASMonths), "\n",
    paste(MetData, collapse = "\n"), "\n",
    PolNum, "\n",
    NameTemp, "\n",
    PolRat, "\n",
    PolDur, "\n",
    paste(RelStart, collapse = " "), "\n",
    GridNum <- PolNum, "\n",
    paste(CenterLatLon, collapse = " "), "\n",
    paste(Spacing, collapse = " "), "\n",
    paste(Span, collapse = " "), "\n",
    paste(OutputDir, collapse = " "), "\n",
    paste(OutputName, collapse = " "), "\n",
    paste(Layers, collapse = "\n"), "\n",
    paste(SampleStart, collapse = " "), "\n",
    paste(SampleStop, collapse = " "), "\n",
    paste(Method, collapse = " "), "\n",
    (DepoNum <- PolNum), "\n",
    paste(ChemParams1, collapse = " "), "\n",
    paste(ChemParams2, collapse = " "), "\n",
    paste(ChemParams3, collapse = " "), "\n",
    paste(ChemParams4, collapse = "\n"), "\n",
sep='', file = "CONTROL")

system("hycs_std.exe")
system(paste("con2asc.exe", OutputName, collapse = " "))
file.remove(OutputName)
ModOutputName <- paste(OutputName, if (j+1 <= 9) {"_00"} else {"_0"},j+1,"_00", sep = "", collapse = "")

FinalDestination <- paste(ModelType,"-","January",collapse = "")

file.rename(paste(getwd(),ModOutputName,collapse = ""), paste(getwd(),FinalDestination,OutputName,collapse = ""))

}


