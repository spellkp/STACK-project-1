dd2 <- melt(abs(log10(Grid1)))
names(dd2) <- c('x','y','Concentration')

d2 <- ggplot(dd2, aes(x,y,z='Concentration'))
d2 + geom_tile(aes(fill=Concentration))  + scale_fill_gradient2(low="white", high="red")




dd2 <- melt(abs(log10(Grid2)))
names(dd2) <- c('x','y','Concentration')

d2 <- ggplot(dd2, aes(x,y,z='Concentration'))
d2 + geom_tile(aes(fill=Concentration))  + scale_fill_gradient2(low="white", high="red")
