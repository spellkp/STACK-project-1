library(ggplot2)
library(ggmap)

JECeGRIDLoc <- c(39.2865, -96.1172)
JSCeGRIDLoc <- c(36.9981, -84.5919)
TCGeGRIDLoc <- c(46.7559, -122.8598)

PlantLocations <- data.frame(c(39.2865, 36.9981, 46.7559), c(-96.1172, -84.5919, -122.8598))
names(PlantLocations) <- c("Lat", "Long")

test <- get_map(location = c(-100, 40), maptype = "satellite", zoom = 4)
ggmap(test) +
  scale_x_continuous(limits = c(-125, -73), expand = c(0, 0)) +
  scale_y_continuous(limits = c(28, 50), expand = c(0, 0)) +
  geom_point(data = PlantLocations, aes(x = Long, y = Lat), size = 5, color = "red") +
  ggtitle("Locations of Selected Power Plants in the United States") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Longitude") +
  ylab("Latitude")
