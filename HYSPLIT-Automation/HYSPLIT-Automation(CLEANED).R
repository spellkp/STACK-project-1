#-----------------------------------------------------------------------------------------------#
# HYSPLIT-FullAutomation                                                                        #
# Below is a first attempt at a full automation of a sensitivity analysis for any location(s).  #
# Any year greater than 2000.                                                                   #
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
    LocationInformation[i, 2] <- as.numeric(readline(paste(prompt = "What is the total eGRID emission value for", LocationInformation[i,1], "in kilograms ?", " ", sep = " ")))
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
ChemicalParameters4 <- as.numeric(ParticleRadioactive)
ChemicalParameters5 <- as.numeric(ParticleResuspensionFactor)


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

##### Run from here if parameters are already entered #####

# Check that all 3 required files are present
if(file.exists("SETUP.CFG") & file.exists("EMITIMES") & file.exists("CONTROL") != TRUE) {
    
    print("SETUP, EMITIMES, or CONTROL file missing")
    stopifnot(file.exists("SETUP.CFG") & file.exists("EMITIMES") & file.exists("CONTROL"))
    
} else {}

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
    
    # The *_StackParams file will be needed for each point source.
    for(i in 1:NumberOfLocations) {     # Starts specific "Stack Information" for each location
      
      eval(parse(text = paste("StackInfo", "<- ", LocationInformation[i,1], "_StackParams", sep = "")))
      
      MeanLocation <- c( mean(StackInfo[,1]), mean(StackInfo[,2]) )
      
      for(q in 1:12) {     # Starts the loop for each month
        
          for(m in 1:MonthData[q,2]) {     # Starts the loop for each day of the month
          
              cat(
            
                  paste(StartYear - 2000, q, m, 00, collapse = " "),"\n",
                  if(ModType == "A") {1} else {NumberOfLocations}, "\n",
            
                  sep = "", file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = "")
            
              )

              # This break in the CONTROL file is where multiple stacks (if applicaple) get added.
              # This is achieved by appending the portion of the CONTROL file generated from the above code.
          
              if(ModType == "A") {
            
                  line <- paste(LocationInformation[i,4], LocationInformation[i,5])
                  write(line, file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = ""), append = TRUE)
            
              if(file.exists("SETUP.CFG") == TRUE) {
              
                  file.rename("SETUP.CFG", "NO_SETUP.CFG")
              
              } else {}
            
              } else if(ModType == "B") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(j in 1:nrow(StackInfo)) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], 0, StackInfo[j,4], StackInfo[j,5], StackInfo[j,6], sep = " ")
                      write(line, file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = ""), append = TRUE)
              
                  }
            
              } else if(ModType == "C") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(j in 1:nrow(StackInfo)) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], 0, StackInfo[j,6], sep = " ")
                      write(line, file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = ""), append = TRUE)
              
                  }
            
              } else if(ModType == "D") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(i in 1:nrow(StackInfo)) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], StackInfo[j,5], 0, sep = " ")
                      write(line, file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = ""), append = TRUE)
              
                  }
            
              } else if(ModType == "E") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(i in 1:nrow(StackInfo)) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], StackInfo[j,3], StackInfo[j,4], StackInfo[j,5], StackInfo[j,6], sep = " ")
                      write(line, file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = ""), append = TRUE)
              
                }
            
              } else if(ModType == "F") {
            
                  if(file.exists("NO_SETUP.CFG") == TRUE) {file.rename("NO_SETUP.CFG", "SETUP.CFG")} else {}
            
                  for(i in 1:nrow(StackInfo)) {
              
                      line <- paste(StackInfo[j,1], StackInfo[j,2], 0, StackInfo[j,4], 0, 0, sep = " ")
                      write(line, file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = ""), append = TRUE)
              
                  }
            
              }     # This closes StackInfo
          
          
          # The remaining parameters of the CONTROL file are added here by appending the portion of the CONTROL file generated above.
          
          y = 1
          cat(
            
              24, "\n",     # Total run time (hrs)
              0, "\n",      # Method of vertical motion
              20000, "\n",  # Top of the model (m)
              3, "\n",      # Number of NAM12km files loaded in
            
              paste(NAMpath), "\n",
              paste(if(q == 1 & m == 1) {paste(StartYear-1, "1231_nam12", sep = "")} else {MeteorologyFileNames[y-1]}), "\n",
            
              paste(NAMpath), "\n",
              paste(MeteorologyFileNames[y]), "\n",
            
              paste(NAMpath), "\n",
              paste(if(q == 12 & m == 31) {paste(StartYear+1, "0101_nam12", sep = "")} else {MeteorologyFileNames[y+1]}), "\n",
            
              1, "\n",      # Number of pollutants
              Pollutant, "\n",
              LocationInformation[i,2]/sum(MonthData[,2]), "\n",
              24, "\n",
              paste(StartYear - 2000, q, m, 0, 0, collapse = " "), "\n",
              1, "\n",      # Number of grids = number of pollutants
              paste(round(mean(StackInfo[,1]), 5), round(mean(StackInfo[,2]), 5), collapse = " "), "\n",
              paste( c(0.05, 0.05), collapse = " "), "\n",     # Resolution of the grid (lat, lon)
              paste( c(80.0, 80.0), collapse = " "), "\n",     # Size of the display grid (lat, lon)
              paste("./", TemporaryDirectory, sep = "", collapse = " "), "\n",    # Save the files here
              paste(LocationInformation[i,1], "-", ModType, "-", StartYear - 2000, "-", q, "-", m, sep = ""), "\n",    # This is the individual file name
            
              paste( c(1, 20000), collapse = " "), "\n",       # Vertical levels, top of model
              paste(StartYear - 2000, q, m, 0, 0, collapse = " "), "\n",
            
              # This conditionals adjusts the model stop date at the end of each month
              paste(        
              
              (if(q == 12 & m == MonthData[12,2]) {temp <- as.numeric((StartYear + 1)) - 2000} else {StartYear - 2000}),
              
              (if(m <= (MonthData[q,2]-1)) {q}
               else if(m == MonthData[q,2] & q != 12) {q+1}
               else if(q == 12 & m == MonthData[12,2]) {1}),
              
              (if (m <= (MonthData[q,2]-1)) {m+1} else {1}),
              00, 00, collapse = " "), "\n",
              # END stop date management
            
              paste( c(00, 24, 00), collapse = " "), "\n",     # Analysis method - averaging
              1, "\n",      # Number of particles for deposition
              paste(ChemicalParameters1, collapse = " "), "\n",
              paste(ChemicalParameters2, collapse = " "), "\n",
              paste(ChemicalParameters3, collapse = " "), "\n",
              paste(ChemicalParameters4, collapse = " "), "\n",
              paste(ChemicalParameters5, collapse = " "), "\n",
            
              sep = "", file = paste(LocationInformation[i, 1], "-", ModType, "-", q, "-", m, sep = ""), append = TRUE
            
          )
          
          # EMITIMES GOES HERE
          
          # HYSPLIT MODEL GOES HERE
          
          # CONVERSION GOES HERE (binary to ascii)
          
        }     # Closes the day
      }     # Closes the Month
    }     # Closes LocationInformation
}     # Closes ModelType


# APPEND EACH SET OF OUTPUTS HERE

# DELETE TEMPORARY DIRECTORY HERE

# RUN MRS MEASURE

# GENERATE PLOTS
    
setwd("..")        
  
