##### Read in Data #####
Model1 <- read.csv("JEC-A-2012")
Model2 <- read.csv("JEC-E-2012")

##### Set StepSize #####
SS <- 0.05


##### Model 1 #####
Area1 = NULL
Area2 = NULL

for (i in 2:366) {

tempModel1 <- subset(Model1, Model1$Day == i)
tempModel2 <- subset(Model2, Model2$Day == i)

Mod1 <- matrix(ncol = ((max(tempModel1$Lon)-min(tempModel1$Lon))/SS) + 1, nrow = ((max(tempModel1$Lat)-min(tempModel1$Lat))/SS) + 1)
Mod2 <- matrix(ncol = ((max(tempModel2$Lon)-min(tempModel2$Lon))/R) + 1, nrow = ((max(tempModel2$Lat)-min(tempModel2$Lat))/R) + 1)

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

Area1[i] <- sum(Mod1)
Area2[i] <- sum(Mod2)

}

round(200*(Area2 - Area1)/(Area2 + Area1), 2)
