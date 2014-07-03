library(plyr)
library(spatial)
library(akima)
library(fields)

#surface plots
c1$A=as.factor(c1$A)
plotcon <- ddply(c1, .(X, Y), summarize, meanV = mean(V, na.rm = TRUE), medianV = median(V, na.rm = TRUE))

s1$A=as.factor (s1$A)
plotsi <- ddply(s1, .(X, Y), summarize, meanV = mean(V, na.rm = TRUE), medianV = median(V, na.rm = TRUE))

##medianV
topo.meter.con <- surf.ls(6, plotcon$X,plotcon$Y,plotcon$medianV)
summary(plotcon)
topo.meter.surface.con <- trmat(topo.meter.con, 0, max(plotcon$X), 0, max(plotcon$Y), 50)
image.plot(topo.meter.surface.con, col=heat.colors(12))
contour(topo.meter.surface.con, add = TRUE)

topo.meter.si <- surf.ls(6, plotsi$X,plotsi$Y,plotsi$medianV)
summary(plotsi)
topo.meter.surface.si <- trmat(topo.meter.si, 0, max(plotsi$X), 0, max(plotsi$Y), 50)
image.plot(topo.meter.surface.si, col=heat.colors(12))
contour(topo.meter.surface.si, add = TRUE)

##meanV (use mean)
topo.meter.con <- surf.ls(6, plotcon$X,plotcon$Y,plotcon$meanV)
summary(plotcon)
topo.meter.surface.con <- trmat(topo.meter.con, 0, max(plotcon$X), 0, max(plotcon$Y), 50)
image.plot(topo.meter.surface.con, col=heat.colors(12))
contour(topo.meter.surface.con, add = TRUE)

topo.meter.si <- surf.ls(6, plotsi$X,plotsi$Y,plotsi$meanV)
summary(plotsi)
topo.meter.surface.si <- trmat(topo.meter.si, 0, max(plotsi$X), 0, max(plotsi$Y), 50)
image.plot(topo.meter.surface.si, col=heat.colors(12))
contour(topo.meter.surface.si, add = TRUE)


##meanV (use mean)
topo.meter.con <- surf.ls(6, plotcon$X,plotcon$Y,plotcon$meanV)
summary(plotcon)
topo.meter.surface.con <- trmat(topo.meter.con, 0, max(plotcon$X), 0, max(plotcon$Y), 50)
image(topo.meter.surface.con)
contour(topo.meter.surface.con, add = TRUE)

x = plotcon$X
y = plotcon$Y
z = plotcon$meanV
akima.li <- interp(x, y, z,  xo=seq(min(x), max(x), length = 30), yo=seq(min(y), max(y), length = 30))
image(akima.li, add=FALSE)
contour(akima.li, add=TRUE)

topo.meter.si <- surf.ls(6, plotsi$X,plotsi$Y,plotsi$meanV)
summary(plotsi)
topo.meter.surface.si <- trmat(topo.meter.si, 0, max(plotsi$X), 0, max(plotsi$Y), 50)
image(topo.meter.surface.si)
contour(topo.meter.surface.si, add = TRUE)

x = plotsi$X
y = plotsi$Y
z = plotsi$medianV
akima.li <- interp(x, y, z,  xo=seq(min(x), max(x), length = 20), yo=seq(min(y), max(y), length = 20))
image(akima.li, add=TRUE)
contour(akima.li, add=TRUE)

