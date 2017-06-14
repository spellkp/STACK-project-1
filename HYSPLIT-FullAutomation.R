#-----------------------------------------------------------------------------------------------#
# HYSPLIT-FullAutomation                                                                        #
# Below is a first attempt at a full automation of a sensitivity analysis for any location(s).  #
# Any year greater than 2000.
#-----------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------#
# Immediately Run Script - User is prompted for all required inputs. #
#--------------------------------------------------------------------#



# Ask user for the year of interest, number of locations to be considered and the information at each location.
Input <- FALSE
while(Input == FALSE) {

    if( interactive() ) {

       EDASpath <- readline(prompt = "This script uses EDAS - 40km data. Include the EDAS directory here - ")  
  
       StartYear <- as.numeric(readline(prompt = "Input Year (YYYY > 2000) - "))
        
           # For incorrect years
           if(StartYear < 2000) {readline(prompt = "Not a valid year... Try again... -_- ")} else {}
    
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
}


ChemicalParameters1 <- as.numeric(c(ParticleDiameter, ParticleDensity, 1))
ChemicalParameters2 <- as.numeric(c(ParticleDepoVelocity, ParticleMolecularWeight, ParticleARatio, ParticleDRatio, ParticleHenry))
ChemicalParameters3 <- as.numeric(c(ParticleHenryConstant, ParticleInCloud, ParticleBelowCloud))
ChemicalParameters4 <- as.numeric(c(ParticleRadioactive, ParticleResuspensionFactor))


# Non-user inputs

StartTime <- c(StartYear - 2000, q, j, 00, 00)
CenterLatitudeLongitude <- c( mean(), mean() )

cat(
  
    paste(StartTime[1:4], collapse = " "),"\n",
    NumberOfLocations,"\n",
    
    # STACK INFORMATION GOES HERE
    
    24, "\n",     # Total run time (hrs)
    0, "\n",      # Method of vertical motion
    20000, "\n",  # Top of the model (m)
    2, "\n",      # Number of EDAS files loaded in
    #EDAS path 1
    #EDAS path 2
    1, "\n",      # Number of pollutants
    Pollutant, "\n",
    
    #eGRID EMISSION VALUE
    
    24, "\n",
    paste(StartTime, collapse = " "), "\n",
    1, "\n",      # Number of grids = number of pollutants
    
    #CENTER LATITUDE AND LONGITUDE
    
    paste( c(0.05, 0.05), collapse = " "), "\n",     # Resolution of the grid (lat, lon)
    paste( c(80.0, 80.0), collapse = " "), "\n",     # Size of the display grid (lat, lon)
    
    #OUTPUT DIRECTORY
    
    #OUTPUT NAME
    
    paste( c(1, 20000), collapse = " "), "\n",       # Vertical levels, top of model
    paste(StartTime, collapse = " "), "\n",
    
    #SAMPLE STOP
    
    paste( c(00, 24, 00), collapse = " "), "\n",     # Analysis method - averaging
    1, "\n",      # Number of particles for deposition
    paste(ChemicalParameters1, collapse = " "), "\n",
    paste(ChemicalParameters2, collapse = " "), "\n",
    paste(ChemicalParameters3, collapse = " "), "\n",
    paste(ChemicalParameters4, collapse = " "), "\n",
    
    sep = "", file = "deleteme"
    
)













