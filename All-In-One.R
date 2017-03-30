#Full Run Metrics
#Select the files you wish to compare. Note that both must come from the same point source and the input files must be in *.CSV format.
#You must specify the location(s) of stacks using Latitude and Longitude coordinates as well as the reported eGRID location.

StackLoc <- rbind(
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618)
  )

eGRIDLoc <- c(39.2865, -96.1172)

Model1 <-     "JEC-A-2012"     #Name of dataset for Model 1; MUST BE CSV
FullModel <-  "JEC-E-2012"     #Name of dataset for Model 2; MUST BE CSV

A <- 2                         #Beginning day
B <- 366                       #Finishing day

SS <- 0.05                     #Spatial Resolution for the area calculations
binwidth <- 100                #Set the binwidth for the concentration profile

#Do you with to use multiple cores? 1 = "Yes", 0 = "No"
#How many cores do you wish to remain "free"?
IfPar <- 0
FreeCores <- 1



#NO FURTHER USER INPUT NEEDED!
#Check the above parameter for accuracy. Also, check that the files to be used are in your working directory.
#check for the installation of all packages that are used.
if (!'ggplot2' %in% installed.packages()) install.packages("ggplot2")
if (!'dplyr' %in% installed.packages()) install.packages("dplyr")
if (!'geosphere' %in% installed.packages()) install.packages("geosphere")
if (!'foreach' %in% installed.packages()) install.packages("foreach")
if (!'doParallel' %in% installed.packages()) install.packages("doParallel")
if (!'ggmap' %in% installed.packages()) install.packages("ggmap")
if (!'akima' %in% installed.packages()) install.packages("akima")

if (IfPar == 1)
{cl <- detectCores()-FreeCores
registerDoParallel(cl)
} else {print("Serial Computation")}


#Construct the Metric Data Frame
Model1 <- read.csv(Model1)[2:6]
Model2 <- read.csv(FullModel)[2:6]

SS <- 0.05

if(is.null(eGRIDLoc)) {eGRIDLoc <- c(mean(StackLoc[,1]), mean(StackLoc[,2]))} else {eGRIDLoc <- eGRIDLoc}

MetricConstruct =
  foreach (i in A:B, .combine = rbind) %dopar% {
   
    #Read the models day-by-day in this loop.
    tempModel1 <- subset(Model1, Model1$Day == i)
    tempModel2 <- subset(Model2, Model2$Day == i)
    Area1 = NULL
    Area2 = NULL
    
      #Determine maximum concentration for each model
      MaxConc1 <- max(tempModel1$Conc)
      MaxConc2 <- max(tempModel2$Conc)
      
      Index1 <- which(tempModel1$Conc == max(tempModel1$Conc))
      Index2 <- which(tempModel2$Conc == max(tempModel2$Conc))
      Disty1 <- 111*(tempModel1$Lat[Index1] - eGRIDLoc[1])
      Distx1 <- 111*(tempModel1$Lon[Index1] - eGRIDLoc[2])
      Disty2 <- 111*(tempModel2$Lat[Index2] - mean(StackLoc[,1]))
      Distx2 <- 111*(tempModel2$Lon[Index2] - mean(StackLoc[,2]))
                     
      
      #Concentration Profile
      RadModel1 <- data.frame(sqrt(tempModel1$Lat - eGRIDLoc[1])^2 + (tempModel1$Lon - eGRIDLoc[2]^2), tempModel1$Conc)
      RadModel2 <- data.frame(sqrt((tempModel2$Lat - mean(StackLoc[,1]))^2 + (tempModel2$Lon - mean(StackLoc[,2]))^2), tempModel2$Conc)
      names(RadModel1) <- c('Radius', 'Concentration')
      names(RadModel2) <- c('Radius', 'Concentration')
      
      binwidth <- max(max(RadModel1$Radius), max(RadModel2$Radius))/100
      
      ConcProf1 = NULL
      ConcProf2 = NULL
      
      for (j in 0:99) {
        
        ConcProf1[j] <- mean(RadModel1$Conc[RadModel1$Radius > j*binwidth & RadModel1$Radius <= (j+1)*binwidth])
        ConcProf2[j] <- mean(RadModel2$Conc[RadModel2$Radius > j*binwidth & RadModel2$Radius <= (j+1)*binwidth])
      }
      
      ConcDiff <- mean(na.omit(200*(ConcProf2 - ConcProf1)/(ConcProf2 + ConcProf1)))   
      
      
      #Determine the areas of each emission. THIS MUST BE THE LAST METRIC!
      Mod1 <- matrix(ncol = ((max(tempModel1$Lon)-min(tempModel1$Lon))/SS) + 1,
                     nrow = ((max(tempModel1$Lat)-min(tempModel1$Lat))/SS) + 1)
      Mod2 <- matrix(ncol = ((max(tempModel2$Lon)-min(tempModel2$Lon))/SS) + 1,
                     nrow = ((max(tempModel2$Lat)-min(tempModel2$Lat))/SS) + 1)
      
      tempModel1$Lon <- (tempModel1$Lon - min(tempModel1$Lon))/SS
      tempModel1$Lat <- (tempModel1$Lat - min(tempModel1$Lat))/SS
      tempModel2$Lon <- (tempModel2$Lon - min(tempModel2$Lon))/SS
      tempModel2$Lat <- (tempModel2$Lat - min(tempModel2$Lat))/SS
      
      
      for (j in 1:nrow(Mod1)) {
        for (k in 1:ncol(Mod1)) {
          
          temp <- mean(tempModel1$Conc[tempModel1$Lon >= k & tempModel1$Lon >= k + 1 & tempModel1$Lat >= j & tempModel1$Lat >= j + 1])
          
          if(is.nan(temp)) {Mod1[j,k] <- 0} else {Mod1[j,k] <- 1}
          
        }
      }
      
      
      
      for (j in 1:nrow(Mod2)) {
        for (k in 1:ncol(Mod2)) {
          
          temp <- mean(tempModel2$Conc[tempModel2$Lon >= k & tempModel2$Lon >= k + 1 & tempModel2$Lat >= j & tempModel2$Lat >= j + 1])
          
          if(is.nan(temp)) {Mod2[j,k] <- 0} else {Mod2[j,k] <- 1}
          
        }
        
      }
      
    cbind(Distx1, Disty1, MaxConc1, Distx2, Disty2, MaxConc2, ConcDiff, sum(Mod1), sum(Mod2))
    
  }

MetricConstruct <- as.data.frame(MetricConstruct)
    
        