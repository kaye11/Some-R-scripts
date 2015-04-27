library(gdata)
library(data.table)
library(ggplot2)

M=0.007812316 #in µmoles
D=10^-5 #in cm2/s
rad=rep(seq(0.0001, 0.0340, 0.0001), each=21) #in cm
time=seq(1, 601, 30) # in sec
var=as.data.frame(cbind(M,D,rad,time))
var$xd= (-var$rad^2)/(4*D*var$time)
e=2.718282
var$rad2=var$rad/0.0001

var$C = M*((4*pi*D*var$time)^(-1.5))*(e^var$xd) #µM

ggplot(data = var, aes(x = rad2, y = C)) + geom_point() + geom_line() + facet_wrap(~time, scales="free")