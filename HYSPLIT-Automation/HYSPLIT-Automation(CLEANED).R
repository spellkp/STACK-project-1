#-----------------------------------------------------------------------------------------------#
# HYSPLIT-FullAutomation                                                                        #
# Below is a first attempt at a full automation of a sensitivity analysis for any location(s).  #
# Any year greater than 2000.
#-----------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------#
# Immediately Run Script - User is prompted for all required inputs. #
#--------------------------------------------------------------------#



# Ask user for the year of interest, number of locations to be considered and the information at each location.
if( interactive() ) {
  
  NAMpath <- readline(prompt = "This script uses NAM - 12km data. Include the NAM directory here - ")  
  
  StartYear <- as.numeric(readline(prompt = "Input Year (YYYY > 2000) - "))
  
  NumberOfLocations <- as.numeric(readline(prompt = "Enter the number of emissions sources - "))
  
  # initialize an empty dataframe
  LocationInformation <- data.frame()
  
  for(i in 1:NumberOfLocations) {
    
    LocationInformation[i, 1] <- readline(paste(prompt = "Provide a three letter title for location", i, "-", " ", sep = " "))
    LocationInformation[i, 2] <- as.numeric(readline(paste(prompt = "What is the eGRID emission value for", LocationInformation[i,1],"?", " ", sep = " ")))
    LocationInformation[i, 3] <- as.numeric(readline(paste(prompt = "How many exhaust points are there at location", LocationInformation[i,1], "?", " ", sep = " ")))
    LocationInformation[i, 4] <- as.numeric(readline(paste(prompt = "What is the eGRID latitude value for", LocationInformation[i,1],"?", " ", sep = " ")))
    LocationInformation[i, 5] <- as.numeric(readline(paste(prompt = "What is the eGRID longitude value for", LocationInformation[i,1],"?", " ", sep = " ")))
    
  }
  
  names(LocationInformation) <- c("Name", "eGRID_Emissions", "Exhaust_Points", "eGRID_Lat", "eGRID_Lon")
  
  for(i in 1:NumberOfLocations) {
    
    StackParams <- data.frame()
    
    for(j in 1:LocationInformation[i, 3]) {
      
      Latitude <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                            "Please provide the stack latitude (deg) - ", sep = "")))
      
      Longitude <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                             "Please provide the stack longitude (deg) - ", sep = "")))
      
      Height <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                          "Please provide the stack height (m) - ", sep = "")))
      
      EmisRate <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                            "Please provide the stack emission rate (kg/hr) - ", sep = "")))
      
      Area <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                        "Please provide the stack area (m^2) - ", sep = "")))
      
      Heat <- as.numeric(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ". ", "\n",
                                        "Please provide the net stack heat (W) - ", sep = "")))
      
      temp <- c(Latitude, Longitude, Height, EmisRate, Area, Heat)
      
      StackParams <- rbind(StackParams, temp)
    }
    
    names(StackParams) <- c("Latitude", "Longitude", "Height", "EmisRate", "Area", "Heat")
    
    assign(noquote(paste(LocationInformation[i, 1], "_StackParams", sep = "")), StackParams)
    
  }
  
  
  
  Pollutant <- readline(prompt = "Enter pollutant name (XXXX) - ")
  
  ParticleDiameter <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                     "Please provide the particle diameter (um) - ", sep = ""))
  
  ParticleDensity <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                    "Please provide the particle density (g/cc) - ", sep = ""))
  
  ParticleDepoVelocity <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                         "Please provide the deposition velocity (m/s) - ", sep = ""))
  
  ParticleMolecularWeight <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                            "Please provide the particle molecular weight (g) - ", sep = ""))
  
  ParticleARatio <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                   "Please provide the particle A-Ratio - ", sep = ""))
  
  ParticleDRatio <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                   "Please provide the particle D-Ratio - ", sep = ""))
  
  ParticleHenry <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                  "Please provide the 'Henry' - ", sep = ""))
  
  ParticleHenryConstant <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                          "Please provide the value for Henry's Constant (M/a) - ", sep = ""))
  
  ParticleInCloud <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                    "Please provide the In-cloud value (l/l) - ", sep = ""))
  
  ParticleBelowCloud <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                       "Please provide the Below-Cloud value (1/s) - ", sep = ""))
  
  ParticleRadioactive <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                        "Please provide the particle halflife (days) - ", sep = ""))
  
  ParticleResuspensionFactor <- readline(paste(prompt = "Information is needed about the particle species ", Pollutant, ". ", "\n",
                                               "Please provide the pollutant resuspension factor (1/m) - ", sep = ""))
  
  
  # Insert a parameter checking method here!
  
}

# User information completed


ChemicalParameters1 <- as.numeric(c(ParticleDiameter, ParticleDensity, 1))
ChemicalParameters2 <- as.numeric(c(ParticleDepoVelocity, ParticleMolecularWeight, ParticleARatio, ParticleDRatio, ParticleHenry))
ChemicalParameters3 <- as.numeric(c(ParticleHenryConstant, ParticleInCloud, ParticleBelowCloud))
ChemicalParameters4 <- as.numeric(c(ParticleRadioactive, ParticleResuspensionFactor))


# Constructing the time management dataframe
MonthNames <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
DaysInMonth <- c(31, if (StartYear %% 4 == 0) {29} else{28}, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
MonthData <- data.frame(MonthNames, DaysInMonth)


# Constructing a vector of meteorology file names
MeteorologyFileNames <- NULL
c <- 1
for(a in 1:12) {
  
    for(b in 1:MonthData[a,2]) {
    
        MeteorologyFileNames[c] <- paste(StartYear, if(a <= 9) {"0"} else {}, a, if(b <= 9) {"0"} else {}, b, "_nam12", sep = "")
        c <- c + 1
        
    }
}


# Create a temporary directory to store all working and temp files
TemporaryDirectory <- paste("I WILL REMOVE MYSELF - ", date(), sep = "")
dir.create(TemporaryDirectory)
setwd(TemporaryDirectory)


# LOOP MODEL TYPE (A-F)
ModelType <- c("A", "B", "C", "D", "E", "F")

# A- eGRID Model
# B- 0m Stack Height
# C- 0m Stack Diameter
# D- 0W Net Heat (Om/s Exit Velocity)
# E- "Full" Model
# F- Modified eGRID Model (with EMITIMES file)


### Constructing the CONTROL file for HYSPLIT ###

# Model type to be used
for(z in 1:6) {     # Begins the "Model Type" loop
  
  ModType <- ModelType[z]
  
