#Loop HYSPLIT for an entire year!- 24hr Dispersions
#Dustin Roten

ModelType <- "E"      #A - EPA data used (only), B - 0m Stack Height, C - 0m/s Emissions, D - 0m^2 area, E - Full Model

for (j in 1:31) {
  
Start <- c(12, 01, j, 00)     #Input model's start date and time (YY MM DD HH)

NumOfStartLocs <- 3           #Number of starting locations
StartLocInfo1 <- c(39.28684, -96.11821, 174.96, 11.25, 47.17) #Starting Location #1 Parameters (lat, lon, height(AGL), velocity(m/s), area(m^2))
StartLocInfo2 <- c(39.28681, -96.11721, 174.96, 11.57, 47.17) #Starting Location #2 Parameters (lat, lon, height(AGL), velocity(m/s), area(m^2))
StartLocInfo3 <- c(39.28681, -96.11618, 174.96, 10.86, 47.17) #Starting Location #3 Parameters (lat, lon, height(AGL), velocity(m/s), area(m^2))

TotRunTime <- 24  #Total run time (hr)
VertMot <- 0      #Method of vertical motion
TopLvl <- 10000.0 #Upper level of the model

PolNum <- 1                       #Number of pollutant species
NameTemp <- "CO2"                 #Name of pollutant species
PolRat <- 560000                  #Emission rate (mass/hr)
PolDur <- 24                      #Pollutant duration (hr)
RelStart <- c(12, 01, j, 00, 00)  #Pollutant start (YY, MM, DD, HH, MM)

CenterLatLon <- c(39.28681, -96.11721)        #Center the display grid (lat, lon)
Spacing <- c(0.05, 0.05)                      #Resolution of the display grid (lat, lon)
Span <- c(80.0, 80.0)                         #size of the display grid (lat, lon)
OutputDir <- "./"                             #Output directory
OutputName <- paste(ModelType, Start[1], Start[2], Start[3], Start[4], sep = "-", collapse = NULL) #output name (YY-MM-DD-HH)

Layers <- c(1, 10000)     #Vertical levels, top of model

SampleStart <- c(12, 01, j, 00, 00)        #Start Sample Date (YY MM DD HH MM)
SampleStop <- c(12, (if (j <= 30) {01} else {02}), (if (j <= 30) {j+1} else {1}), 00, 00) #Stop Sample Date (YY MM DD HH MM)
Method <- c(00, 24, 00)    #Method (XX HH MM)

ChemParams1 <- c(0.000257, 0.00197, 1.0)      #Particle Diameter (um), Density (g/cc), Shape
ChemParams2 <- c(0.0, 0.0, 0.0, 0.0, 0.0)     #Deposition Velocity (m/s), Mol Wgt (g), A-Ratio, D-Ratio, Henry
ChemParams3 <- c(101.325, 0.0, 0.0)           #Henry's Constant (M/a), In-cloud (l/l), Below-cloud (1/s)
ChemParams4 <- c(0.0, 0.0)                    #Radioactive Decay - Halflife (days), Pollutant Resuspension Factor (1/m)

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

FinalDestination <- paste(ModelType,"-","January", sep = "", collapse = "")

file.rename(paste(getwd(), "/", ModOutputName, sep = "", collapse = ""), 
          paste(getwd(), "/", FinalDestination, "/", OutputName, sep = "", collapse = ""))

}


