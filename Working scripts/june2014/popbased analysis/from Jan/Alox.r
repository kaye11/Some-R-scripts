library(ggplot2)
library(nlme)
library(gridExtra)

#Data import


nso = read.csv("alox.csv", sep = ";")
str(nso)

#taking the mean value for each track and bin
nso2 <- ddply(nso, .(A,bin,cond), summarize, meanVm = mean(Vm, na.rm = TRUE), medianVm = median(Vm, na.rm = TRUE), meanang = mean(ang, na.rm =TRUE),  meanlen = mean(len, na.rm = TRUE), meandir = mean (dir, na.rm = TRUE))

Vm = qplot(time, Vm, data = nso, color = cond, geom = "smooth") + facet_wrap(~bin)  + theme_classic()

ang = qplot(time, dir, data = nso, color = cond, geom = "smooth") + facet_wrap(~bin)  + theme_classic()

len = qplot(time, len, data = nso, color = cond, geom = "smooth") + facet_wrap(~bin)  + theme_classic()



grid.arrange(Vm, ang, len, ncol=1, nrow = 3)

## Test is i can plot distance from the bead  and swimming ngle of the cells

controldataraw= read.csv("day3control-001.csv", sep =";")
controldataraw$distancetobead = sqrt((455-controldataraw$X)^2 + (377-controldataraw$Y)^2)

controldataraw2 = controldataraw[complete.cases(controldataraw),]
controldata <- ddply(controldataraw2, .(X,Y), summarize, meanangle = mean(angle, na.rm = TRUE))
# In our case the coordinates columns are already called x and y, so we will
# use these

# convert this basic data frame into a spatial points data frame
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


#Mean angle

idw <- idw(formula = meanangle ~ 1, locations = controldata, newdata = grd3)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("X", "Y", "meanangle")
plot <- ggplot(data = idw.output, aes(x = X, y = Y))  + c(geom_tile(data = idw.output, aes(fill = meanangle)))  

ContAngle = plot +  scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits = c(0,180))+ labs(title = "Control") + coord_fixed() +  scale_y_reverse()+annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))






sidataraw= read.csv("day3sibead-002.csv", sep =";")
sidataraw$distancetobead = sqrt((452-sidataraw$X)^2 + (345-sidataraw$Y)^2)

sidataraw2 = sidataraw[complete.cases(sidataraw),]
sidata <- ddply(sidataraw2, .(X,Y), summarize, meanangle = mean(angle, na.rm = TRUE))
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

Siangle = plot +  scale_fill_gradient(low = "#FEEBE2", high = "#7A0177", limits = c(0,180))+ labs(title = "si") + coord_fixed() +  scale_y_reverse()+annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)),                                                                                                                                                            y=377 + 53.5*sin(seq(0,2*pi,length.out=100)))



grid.arrange(ContAngle, Siangle, ncol=2)


c = qplot(T,distancetobead, data = sidataraw2, geom = c( "point","smooth"), color = A, group = A) + labs("Si") 

d = qplot(T,distancetobead, data = controldataraw, geom = c( "point","smooth"),color = A, group = A) + labs("Control")

grid.arrange(d,c, ncol=2)


##distnace over time
c = qplot(T,distancetobead, data = sidataraw2, geom = c( "smooth"), color = A, group = A) + labs(title = "Si") + geom_hline(yintercept = 61.5,colour = "red") + scale_y_continuous(limits =c(0,620))+ scale_x_continuous(limits =c(0,620))

d = qplot(T,distancetobead, data = controldataraw, geom = c( "smooth"),color = A, group = A) + labs(title = "Control") + geom_hline(yintercept = 53.5,colour = "red") + scale_y_continuous(limits =c(0,620))+ scale_x_continuous(limits =c(0,620))

grid.arrange(d,c, ncol=2)



#angle over time. 
qplot(T,angle, data = sidataraw, geom = c( "point","smooth"))
qplot(T,angle, data = controldataraw, geom = c( "point","smooth"))


a = qplot(distancetobead,angle, data = controldataraw, geom = c( "point","smooth"))
b = qplot(distancetobead,angle, data = sidataraw, geom = c( "point","smooth"))
grid.arrange(a, b, ncol=1)
