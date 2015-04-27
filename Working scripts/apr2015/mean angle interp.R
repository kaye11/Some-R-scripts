

controldataraw2 = Con[complete.cases(Con),]
controldata <- ddply(controldataraw2, .(X,Y), summarize, meanangle = mean(angs, na.rm = TRUE))
# In our case the coordinates columns are already called x and y, so we will
# use these

# convert this basic data frame into a spatial points data frame
library(sp)
coordinates(controldata) = ~ X + Y

plot(controldata)

x.range <- as.integer(range(controldata@coords[, 1]))
y.range <- as.integer(range(controldata@coords[, 2]))
plot(controldata)

grd3 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 50), y = seq(from = y.range[1], to = y.range[2], by = 50))
coordinates(grd3) <- ~x + y
gridded(grd3) <- TRUE

plot(grd3, cex = 1.5)
points(controldata, pch = 1, col = "red", cex = 1)
title("control")

library(gstat)

#Mean angle

idw <- idw(formula = meanangle ~ 1, locations = controldata, newdata = grd3)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("X", "Y", "meanangle")
plot <- ggplot(data = idw.output, aes(x = X, y = Y))  + c(geom_tile(data = idw.output, aes(fill = meanangle)))  

ContAngle = plot +  scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits = c(-1,1))+ labs(title = "Control") + coord_fixed() 
+  scale_y_reverse()+annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))







sidataraw2 = Si[complete.cases(Si),]
sidata <- ddply(sidataraw2, .(X,Y), summarize, meanangle = mean(angs, na.rm = TRUE))
# In our case the coordinates columns are already called x and y, so we will
# use these

# convert this basic data frame into a spatial points data frame
coordinates(sidata) = ~ X + Y

plot(sidata)

x.range <- as.integer(range(sidata@coords[, 1]))
y.range <- as.integer(range(sidata@coords[, 2]))
plot(sidata)

grd4 <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 50), y = seq(from = y.range[1], to = y.range[2], by = 50))
coordinates(grd4) <- ~x + y
gridded(grd4) <- TRUE

plot(grd4, cex = 1.5)
points(sidata, pch = 1, col = "red", cex = 1)
title("si")


#Mean angle

idw <- idw(formula = meanangle ~ 1, locations = sidata, newdata = grd4)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("X", "Y", "meanangle")
plot <- ggplot(data = idw.output, aes(x = X, y = Y))  + c(geom_tile(data = idw.output, aes(fill = meanangle)))  

Siangle = plot +  scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits = c(-1,1))+ labs(title = "si") + coord_fixed() +  scale_y_reverse()+annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))



grid.arrange(ContAngle, Siangle, ncol=2)





qplot(T,angle, data = sidataraw, geom = c( "point","smooth"))
qplot(T,angle, data = controldataraw, geom = c( "point","smooth"))

controldataraw$distancetobead = sqrt((455-controldataraw$X)^2 + (377-controldataraw$Y)^2)
sidataraw$distancetobead = sqrt((452-sidataraw$X)^2 + (345-sidataraw$Y)^2)

a = qplot(distancetobead,angle, data = controldataraw, geom = c( "point","smooth"))
b = qplot(distancetobead,angle, data = sidataraw, geom = c( "point","smooth"))
grid.arrange(a, b, ncol=1)