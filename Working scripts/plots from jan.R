##from jan
library(plyr)

st1=c1
str(st1)
st1$A = as.factor(st1$A)

st1sum <- ddply(st1, .(X, Y), summarize, meandeg = mean(angle, na.rm = TRUE),meanV = mean(V, na.rm = TRUE))


ggplot(data = st1sum, aes(X,Y,z =meandeg)) + stat_summary2d()
ggplot(data = st1sum, aes(X,Y,z =meanV)) + stat_summary2d()

##karen version
ggplot(data=st1, aes(X, Y, z=V))+stat_summary2d(mapping=aes(x=X, y=Y, z=V), 
                                                data=st1, bin=50, fun = mean)

ggplot(data=st1, aes(X, Y, z=angle))+stat_summary2d(mapping=aes(x=X, y=Y, z=angle), 
                                                data=st1, bin=50, fun = mean)


slab <- ddply(st1, .(X, Y), summarize, meanV = mean(V, na.rm = TRUE), medianV = median(V, na.rm = TRUE))
s1$A=as.factor (s1$A)
slab2 <- ddply(s1, .(X, Y), summarize, meanV = mean(V, na.rm = TRUE), medianV = median(V, na.rm = TRUE))


library(spatial)
library(akima)
##medianV
topo.meter.ls3 <- surf.ls(6, slab$X,slab$Y,slab$medianV)
summary(slab)
topo.meter.surface3 <- trmat(topo.meter.ls3, 0, max(slab$X), 0, max(slab$Y), 50)
image(topo.meter.surface3)
contour(topo.meter.surface3, add = TRUE)

topo.meter.ls3 <- surf.ls(6, slab2$X,slab2$Y,slab2$medianV)
summary(slab2)
topo.meter.surface3 <- trmat(topo.meter.ls3, 0, max(slab2$X), 0, max(slab2$Y), 50)
image(topo.meter.surface3)
contour(topo.meter.surface3, add = TRUE)


##meanV
topo.meter.ls3 <- surf.ls(6, slab$X,slab$Y,slab$meanV)
summary(slab)
topo.meter.surface3 <- trmat(topo.meter.ls3, 0, max(slab$X), 0, max(slab$Y), 50)
image(topo.meter.surface3)
contour(topo.meter.surface3, add = TRUE)

topo.meter.ls3 <- surf.ls(6, slab2$X,slab2$Y,slab2$meanV)
summary(slab2)
topo.meter.surface3 <- trmat(topo.meter.ls3, 0, max(slab2$X), 0, max(slab2$Y), 50)
image(topo.meter.surface3)
contour(topo.meter.surface3, add = TRUE)

##akima
x = slab$X
y = slab$Y
z = slab$medianV
akima.li <- interp(x, y, z,  xo=seq(min(x), max(x), length = 20), yo=seq(min(y), max(y), length = 20))
image.plot  (akima.li, add=TRUE)
contour(akima.li, add=TRUE)

slab2 <- ddply(silab1, .(X, Y), summarize, meanP = mean(P, na.rm = TRUE), medianP = median(P, na.rm = TRUE))
slab2[is.na(slab2)]<-0

topo.meter.ls3 <- surf.ls(6, slab2$X,slab2$Y,slab2$medianV)
summary(slab2)
topo.meter.surface4 <- trmat(topo.meter.ls3, 0, max(slab2$X), 0, max(slab2$Y), 50)
image(topo.meter.surface4)
contour(topo.meter.surface4, add = TRUE)

