library(gridExtra)
library(ggplot2)
library(sp)
library(maptools)
library(gstat)
library(plyr)


con.bead <- ddply(con.bead, .(X,Y), summarize, meanV = mean(V, na.rm = TRUE), medianV = median(V, na.rm = TRUE))
# In our case the coordinates columns are already called x and y, so we will
# use these

# convert this basic data frame into a spatial points data frame
coordinates(con.bead) = ~ X + Y

plot(con.bead)

x.range <- as.integer(range(con.bead@coords[, 1]))
y.range <- as.integer(range(con.bead@coords[, 2]))
plot(con.bead)

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 20), y = seq(from = y.range[1], to = y.range[2], by = 50))
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5)
points(con.bead, pch = 1, col = "red", cex = 1)
title("control")


#MEDIAN SPEED

idw <- idw(formula = medianV ~ 1, locations = con.bead, newdata = grd)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("X", "Y", "medianV")
plot <- ggplot(data = idw.output, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.output, aes(fill = medianV)))  #then create a tile layer and fill with predicted values

control = plot + layer1 + scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits=c(0, 7))+ 
  labs(title = "Control") + coord_fixed() +  scale_y_reverse()+
  annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)), y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))



#MEAN SPEED
idwmean <- idw(formula = meanV ~ 1, locations = con.bead, newdata = grd)
idw.outputmean = as.data.frame(idwmean)
names(idw.outputmean)[1:3] <- c("X", "Y", "meanV")
plot <- ggplot(data = idw.outputmean, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.outputmean, aes(fill = meanV)))  #then create a tile layer and fill with predicted values

controlmeanspeed = plot + layer1 + scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits=c(0, 7))+ labs(title = "Control") + coord_fixed() +  scale_y_reverse()+annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))



si.bead <- ddply(si.bead, .(X,Y), summarize, meanV = mean(V, na.rm = TRUE), medianV = median(V, na.rm = TRUE))
# In our case the coordinates columns are already called x and y, so we will
# use these

# convert this basic data frame into a spatial points data frame
coordinates(si.bead) = ~ X + Y

plot(si.bead)

x.range <- as.integer(range(si.bead@coords[, 1]))
y.range <- as.integer(range(si.bead@coords[, 2]))
plot(si.bead)

grd2 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 20), y = seq(from = y.range[1], to = y.range[2], by = 50))
coordinates(grd2) <- ~x + y
gridded(grd2) <- TRUE

plot(grd2, cex = 1.5)
points(si.bead, pch = 1, col = "red", cex = 1)
title("Si")
idw <- idw(formula = medianV ~ 1, locations = si.bead, newdata = grd2)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("X", "Y", "medianV")
plot <- ggplot(data = idw.output, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.output, aes(fill = medianV)))  #then create a tile layer and fill with predicted values

silicate = plot + layer1 + scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits=c(0, 7)) + labs(title = "Si") + coord_fixed() +  scale_y_reverse()+annotate("path",                                                                                                                                                x = 452+61.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=345+61.5*sin(seq(0,2*pi,length.out=100)))

control 
silicate

grid.arrange(control, silicate, ncol=2)




#MEAN SPEED
idwmean2 <- idw(formula = meanV ~ 1, locations = si.bead, newdata = grd)
idw.outputmean2 = as.data.frame(idwmean2)
names(idw.outputmean2)[1:3] <- c("X", "Y", "meanV")
plot <- ggplot(data = idw.outputmean2, aes(x = X, y = Y))  #start with the base-plot 
layer1 <- c(geom_tile(data = idw.outputmean2, aes(fill = meanV)))  #then create a tile layer and fill with predicted values

Simeanspeed = plot + layer1 + scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits=c(0, 7))+ labs(title = "Si") + coord_fixed() +  scale_y_reverse() +annotate("path",                                                                                                                                                x = 452+61.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=345+61.5*sin(seq(0,2*pi,length.out=100)))

 


grid.arrange(controlmeanspeed, Simeanspeed, ncol=2)

#Kriging

#control
semivariog <- variogram(medianV ~ 1, locations = con.bead, data = con.bead)
plot(semivariog)
semivariog
model.variog <- vgm(psill = 4, model = "Exp", nugget = 2, range = 300)
fit.variog <- fit.variogram(semivariog, model.variog)
plot(semivariog, fit.variog)
bubble(con.bead, zcol='medianV', fill=FALSE, do.sqrt=FALSE, maxsize=2)


x.range <- as.integer(range(con.bead@coords[, 1]))
y.range <- as.integer(range(con.bead@coords[, 2]))
plot(con.bead)


grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 20), y = seq(from = y.range[1], to = y.range[2], by = 20))
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

krig <- krige(formula = medianV ~ 1, locations = con.bead, newdata = grd, model = model.variog, nmax = 200)

krig.output = as.data.frame(krig)
names(krig.output)[1:3] <- c("X", "Y", "var1.pred")

plot <- ggplot(data = krig.output, aes(x = X, y = Y))  #start with the base-plot and add the Kriged data to it
layer1 <- c(geom_tile(data = krig.output, aes(fill = var1.pred)))  #then create a tile layer and fill with predicted
KrigControl = plot + layer1  + scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits=c(0,8.5)) + labs(title = "Control") + coord_fixed()+  scale_y_reverse()+annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))

KrigControl


#Si treatment krige

semivariog2 <- variogram(medianV ~ 1, locations = si.bead, data = si.bead)
plot(semivariog2)
semivariog2
model.variog2 <- vgm(psill = 2, model = "Exp", nugget = 2, range = 300)
fit.variog2 <- fit.variogram(semivariog2, model.variog2)
plot(semivariog2, fit.variog2)

bubble(si.bead, zcol='medianV', fill=FALSE, do.sqrt=FALSE, maxsize=2)


x.range <- as.integer(range(si.bead@coords[, 1]))
y.range <- as.integer(range(si.bead@coords[, 2]))
plot(si.bead)


grd2 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 20), y = seq(from = y.range[1], to = y.range[2], by = 20))
coordinates(grd2) <- ~x + y
gridded(grd2) <- TRUE

krig2 <- krige(formula = medianV ~ 1, locations = si.bead, newdata = grd2, model = model.variog, nmax = 200)

krig.output2 = as.data.frame(krig2)
names(krig.output2)[1:3] <- c("X", "Y", "var1.pred")

plot2 <- ggplot(data = krig.output2, aes(x = X, y = Y))  #start with the base-plot and add the Kriged data to it
layer1 <- c(geom_tile(data = krig.output2, aes(fill = var1.pred)))  #then create a tile layer and fill with predicted
KrigSi = plot2 + layer1  + scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits=c(0,8.5)) + labs(title = "Si") + coord_fixed()+  scale_y_reverse()+annotate("path",                                                                                                                                                x = 452+61.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=345+61.5*sin(seq(0,2*pi,length.out=100)))

KrigSi

grid.arrange(KrigControl, KrigSi, ncol=2)
