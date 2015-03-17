library(gridExtra)
library(ggplot2)
library(sp)
library(maptools)
library(gstat)
library(plyr)


con.bead <- ddply(con.bead, .(X,Y), summarize, mAngs = mean(angs, na.rm = TRUE), medAngs = median(angs, na.rm = TRUE))
# In our case the coordinates columns are already called x and y, so we will
# use these

# convert this basic data frame into a spatial points data frame
coordinates(con.bead) = ~ X + Y

plot(con.bead)

x.range <- as.integer(range(con.bead@coords[, 1]))
y.range <- as.integer(range(con.bead@coords[, 2]))
plot(con.bead)

grd <- expand.grid(x = seq(from = 0, to = 955, by = 20), y = seq(from = 0, to = 765, by = 20))
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5)
points(con.bead, pch = 1, col = "red", cex = 1)
title("control")


#MEDIAN SPEED

idw <- idw(formula = medAngs ~ 1, locations = con.bead [!is.na(con.bead$medAngs), ], newdata = grd)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("X", "Y", "medAngs")
plot <- ggplot(data = idw.output, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.output, aes(fill = medAngs)))  #then create a tile layer and fill with predicted values

control = plot + layer1 + scale_fill_gradient(low = "ghostwhite", high = "darkblue", limits=c(-1, 1))+ 
  labs(title = "Control") + coord_fixed() +  scale_y_reverse()+
  annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)), y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))



#MEAN SPEED
idwmean <- idw(formula = mAngs ~ 1, locations = con.bead [!is.na(con.bead$mAngs), ], newdata = grd)
idw.outputmean = as.data.frame(idwmean)
names(idw.outputmean)[1:3] <- c("X", "Y", "mAngs")
plot <- ggplot(data = idw.outputmean, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.outputmean, aes(fill = mAngs)))  #then create a tile layer and fill with predicted values

controlmeanspeed = plot + layer1 + scale_fill_gradient(low = "ghostwhite", high = "darkblue", limits=c(-1, 1))+ labs(title = "Control") + coord_fixed() +  scale_y_reverse()+annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))



si.bead <- ddply(si.bead, .(X,Y), summarize, mAngs = mean(angs, na.rm = TRUE), medAngs = median(angs, na.rm = TRUE))
# In our case the coordinates columns are already called x and y, so we will
# use these

# convert this basic data frame into a spatial points data frame
coordinates(si.bead) = ~ X + Y

plot(si.bead)

x.range <- as.integer(range(si.bead@coords[, 1]))
y.range <- as.integer(range(si.bead@coords[, 2]))
plot(si.bead)

grd2 <- expand.grid(x = seq(from = 0, to = 955, by = 20), y = seq(from = 0, to = 765, by = 20))
coordinates(grd2) <- ~x + y
gridded(grd2) <- TRUE



plot(grd2, cex = 1.5)
points(si.bead, pch = 1, col = "red", cex = 1)
title("Si")
idw <- idw(formula = medAngs ~ 1, locations = si.bead [!is.na(si.bead$medAngs), ], newdata = grd2)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("X", "Y", "medAngs")
plot <- ggplot(data = idw.output, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.output, aes(fill = medAngs)))  #then create a tile layer and fill with predicted values

silicate = plot + layer1 + scale_fill_gradient(low = "ghostwhite", high = "darkblue", limits=c(-1, 1)) + labs(title = "Si") + coord_fixed() +  scale_y_reverse()+annotate("path",                                                                                                                                                x = 452+61.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=345+61.5*sin(seq(0,2*pi,length.out=100)))

control 
silicate

grid.arrange(control, silicate, ncol=2)




#MEAN SPEED
idwmean2 <- idw(formula = mAngs ~ 1, locations = si.bead [!is.na(si.bead$mAngs), ], newdata = grd)
idw.outputmean2 = as.data.frame(idwmean2)
names(idw.outputmean2)[1:3] <- c("X", "Y", "mAngs")
plot <- ggplot(data = idw.outputmean2, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.outputmean2, aes(fill = mAngs)))  #then create a tile layer and fill with predicted values

Simeanspeed = plot + layer1 + scale_fill_gradient(low = "ghostwhite", high = "darkblue", limits=c(-1, 1))+ labs(title = "Si") + coord_fixed() +  scale_y_reverse() +annotate("path",                                                                                                                                                x = 452+61.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=345+61.5*sin(seq(0,2*pi,length.out=100)))

 


grid.arrange(controlmeanspeed, Simeanspeed, ncol=2)

