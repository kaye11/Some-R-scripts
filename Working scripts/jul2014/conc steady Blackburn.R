library(gdata)
library(data.table)
library(ggplot2)

M=0.00145 #in µmoles
D=1e-5 #in cm2/s
rad=rep(seq(0.0001, 0.0340, 0.0001), each=21) #in cm
time=seq(1, 600, 20) # in sec
var=as.data.frame(cbind(M,D,rad,time))
var$xd= (-var$rad^2)/(4*D*var$time)

var$C = M*((4*pi*D*var$time)^(-1.5))*(2.718282^var$xd)

ggplot(data = var, aes(x = rad, y = C)) + geom_point() + geom_line() + facet_wrap(~time, scales="free")