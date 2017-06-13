#-----------------------------------------------------------------------------------------------#
# HYSPLIT-FullAutomation                                                                        #
# Below is a first attempt at a full automation of a sensitivity analysis for any location(s).  #
# Any year greater than or equal to 2000.
#-----------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------#
# Immediately Run Script - User is prompted for all required inputs. #
#--------------------------------------------------------------------#



# Ask user for the year of interest, number of locations to be considered and the information at each location.
if( interactive() ) {

    EDASpath <- readline(prompt = "This script uses EDAS - 40km data. Include the EDAS directory here - ")  
  
    StartYear <- readline(prompt = "Input Year (YYYY >= 2000) - ")
        
        # For incorrect years
        if(StartYear < 2000) {readline(prompt = "Not a valid year... Try again... -_- ")} else {}
    
    NumberOfLocations <- as.numeric(readline(prompt = "Enter the number of emissions sources - "))
  
    # initialize an empty dataframe
    LocationInformation <- data.frame()
  
    for(i in 1:NumberOfLocations) {
    
      LocationInformation[i, 1] <- readline(paste(prompt = "Provide a three letter title for location", i, "-", " ", sep = " "))
      LocationInformation[i, 2] <- readline(paste(prompt = "What is the eGRID emission value for", LocationInformation[i,1],"?", sep = " "))
      LocationInformation[i, 3] <- readline(paste(prompt = "How many exhaust points are there at location", i, "-", " ", sep = " "))

    }
    
    for(i in 1:NumberOfLocations) {
      
      StackParams <- data.frame()
      
        for(j in 1:LocationInformation[i, 3]) {
        
            Latitude <- c(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ".", "\n",
                           "Please provide the stack latitude (deg) - ", sep = "")))
            
            Longitude <- c(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ".", "\n",
                                         "Please provide the stack longitude (deg) - ", sep = "")))
            
            Height <- c(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ".", "\n",
                                         "Please provide the stack height (m) - ", sep = "")))
            
            EmisRate <- c(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ".", "\n",
                                         "Please provide the stack emission rate (kg/hr) - ", sep = "")))
            
            Area <- c(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ".", "\n",
                                         "Please provide the stack area (m^2) - ", sep = "")))
            
            Heat <- c(readline(paste(prompt = "Information is needed for stack", " ", j, " ", "at plant", " ", LocationInformation[i, 1], ".", "\n",
                                         "Please provide the net stack heat (W) - ", sep = "")))
      
            temp <- c(Latitude, Longitude, Height, EmisRate, Area, Heat)
            
            StackParams <- rbind(StackParams, temp)
        }
      
        names(StackParams) <- c("Latitude", "Longitude", "Height", "EmisRate", "Area", "Heat")
        
        assign(paste(LocationInformation[i, 1],"-",StackParams, sep = ""))
          
     }
  
  Pollutant <- readline(prompt = "Enter pollutant name (XXXX) - ")
  
  
  StartYear <- as.numeric(StartYear)

}

