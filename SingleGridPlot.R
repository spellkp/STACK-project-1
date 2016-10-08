dd2 <- melt(Grid1)

div1 <- (max(dd2$value)-min(dd2$value))/6
a <- 0
b <- 1.00e-16
c <- 1.00e-15
d <- 1.00e-14
e <- 1.00e-13
f <- 1.00e-12
g <- 1.00e-11



dd2$cutdata <- cut(dd2$value, breaks = c(a, b, c, d, e, f, g))

vals=c(a:b, b:c, c:d, d:e, e:f, f:g)

d2 <- ggplot(dd2, aes(Var1, Var2, cutdata))
d2 + geom_tile(aes(fill=cutdata)) +
  scale_colour_gradientn(colors = "green") +
  labs(x = "0.1 Degrees", y = "0.1 Degrees", title = "Gridded Dispersion Concentration 0m AGL") +
  guides(fill=guide_legend(title="Concentration"))
  
##########################################################

dd3 <- melt(Grid2)

div2 <- (max(dd3$value)-min(dd3$value))/6

dd3$cutdata <- cut(dd3$value, breaks = c(a, b, c, d, e, f, g))

d3 <- ggplot(dd3, aes(Var1, Var2, cutdata))
d3 + geom_tile(aes(fill=cutdata)) +
  scale_colour_gradientn(colours = c("firebrick4","orange","yellow", "green", "darkblue", "purple")) +
  labs(x = "0.1 Degrees", y = "0.1 Degrees", title = "Gridded Dispersion Concentration 200m AGL", legend = "Concentration") +
  guides(fill=guide_legend(title="Concentration"))
