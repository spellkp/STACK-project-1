dd2 <- melt(Grid1)

div1 <- (max(AGL0m$CO2)-min(AGL0m$CO2))/6
a <- min(AGL0m$CO2)
b <- min(AGL0m$CO2) + div1
c <- b + div1
d <- c + div1
e <- d + div1
f <- e + div1
g <- f + div1

d2 <- ggplot(dd2, aes(x,y,z='Concentration'))
d2 + geom_tile(aes(fill=Concentration)) +
  scale_fill_manual(breaks = c("\[-inf,a)", "\[a,b)", "\[b,c)", "\[c,d)", "\[d,e)", "\[e,f)", "\[f,g)", "\[g,inf)"), values = c("black", "green", "darkgreen", "blue", "darkblue", "red", "darkred", "deepred"))
  
##########################################################

dd3 <- melt(Grid2)
names(dd3) <- c('x','y','Concentration')

div2 <- (max(AGL200m$CO2)-min(AGL200m$CO2))/6
a1 <- min(AGL200m$CO2)
b1 <- min(AGL200m$CO2) + div2
c1 <- b1 + div2
d1 <- c1 + div2
e1 <- d1 + div2
f1 <- e1 + div2
g1 <- f1 + div2

d3 <- ggplot(dd3, aes(x,y,z='Concentration'))
d3 + geom_tile(aes(fill=Concentration))  + 
