ModelType <- "E"
MonthName <- c("January",
               "February",
               "March",
               "April",
               "May",
               "June",
               "July",
               "August",
               "September",
               "October",
               "November",
               "December")

for (i in 1:12) {
  
  filepath <- paste("C:/Users/dusti/Documents/ResearchScripts/STACK-project/JEC-2012/", ModelType, "-", MonthName[i], sep = "")
  files <- dir(path = filepath, pattern = "", full.names = TRUE, recursive = TRUE)
  
for (j in 1:length(files)) {
  
Day <- read.delim(files[j], sep = "", header = TRUE, strip.white = TRUE)
colnames(Day)[1] <- "Day"
colnames(Day)[2] <- "Hour"
colnames(Day)[3] <- "Lat"
colnames(Day)[4] <- "Lon"
colnames(Day)[5] <- "Conc"
Day[,6] <- NULL

  if (j == 1) {Month <- Day}
    
  else{Month <- as.data.frame(rbind(Month, Day))}
}

  write.csv(Month, paste(ModelType, "-", MonthName[i], ".csv", sep = ""))

}