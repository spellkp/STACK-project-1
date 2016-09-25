###### Input Parameters #####

centy <- 39.2846
centx <- -96.1150

AGL0m <- read.delim("0mcdump.txt", header = TRUE, sep = "", dec = ".")
AGL200m <- read.delim("200mcdump.txt", header = TRUE, sep = "", dec = ".")

resolution <- 30

#############################
library(ggplot2)
library(rgl)
library(patchPlot)
library(reshape2)
source("http://www.phaget4.org/R/myImagePlot.R")
t1 <- AGL0m[[3]]
y1 <- AGL0m[[5]]
x1 <- AGL0m[[6]]
Concentration0 <- AGL0m[[7]]
metfile0 <- data.frame(t1, x1, y1, Concentration0)

t2 <- AGL200m[[3]]
y2 <- AGL200m[[5]]
x2 <- AGL200m[[6]]
Concentration1 <- AGL200m[[7]]
metfile1 <- data.frame(t2, x2, y2, Concentration1)

###Bin Generator###

Grid1 = NULL
Grid2 = NULL

minx1 <- round(min(min(x1), min(x2)), 0)-1
maxx1 <- round(max(max(y1), max(y2)), 0)+1

miny1 <- round(min(min(y1), min(y2)), 0)-1
maxy1 <- round(max(max(y1), max(y2)), 0)+1

Grid1 <- matrix(0, nrow = 200, ncol = 200)
Grid2 <- matrix(0, nrow = 200, ncol = 200)

#####

for (i in 1:200) {
  
  for (j in 1:200) {
    
    Grid1[i,j] = sum(metfile0$Concentration0[metfile0$t1 == 4 & metfile0$x1 >= centx+(-10+0.1*j) & metfile0$x1 
                                              <= centx+(-10+0.1*(j+1)) & metfile0$y1 >= centy+(-10+0.1*i) 
                                              & metfile0$y1 <= centy+(-10+0.1*(i+1))])
    
  }
  
}

Grid1[is.nan(Grid1)] <- 0

#####

for (k in 1:200) {
  
  for (l in 1:200) {
    
    Grid2[k,l] = sum(metfile1$Concentration1[metfile1$t2 == 4 & metfile1$x2 >= centx+(-10+0.1*l) & metfile1$x2 
                                              <= centx+(-10+0.1*(l+1)) & metfile1$y2 >= centy+(-10+0.1*k) 
                                              & metfile1$y2 <= centy+(-10+0.1*(k+1))])
    
  }
  
}

Grid2[is.nan(Grid2)] <- 0

#####

GridPlot <- as.matrix(200*(Grid2-Grid1)/(Grid2+Grid1))
GridPlot[is.nan(GridPlot)] <- 0

#myImagePlot(Grid1, title = c("Concentration at 0m AGL"))
#myImagePlot(Grid2, title = c("Concentration at 200m AGL"))

#myImagePlot(GridPlot, title = c("Dispersion % Difference"))
mean(GridPlot)
# rgl.surface(1:(maxx1-minx1), 1:(maxy1-miny1), GridPlot)

dd <- melt(GridPlot)
names(dd) <- c('x','y','Concentration')

d <- ggplot(dd, aes(x,y,z='Concentration'))
d + geom_tile(aes(fill = Concentration))  + scale_fill_gradient2(low="darkgreen", high="red")

