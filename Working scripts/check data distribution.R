library(fBasics)
library(vcd)
library(lmom)

##GRAPHING
hist(nso$Vm,main="Histogram of observed data") #getting a histogram
plot(density(nso$Vm),main="Density estimate of data") #getting a density plot
plot(ecdf(nso$Vm),main= "Empirical cumulative distribution function")

r.norm<-(k-mean(k))/sd(k) ## standardized data 
qqnorm(r.norm) ## drawing the QQplot 
abline(0,1) ## drawing a 45-degree reference line 

##A 45-degree reference line is also plotted. If the empirical data come from the 
#population with the choosen distribution, the points should fall approximately along this 
#reference line. The greater the departure from this reference line, the greater the 
#evidence for the conclusion that the data set have come from a population 
#with a different distribution

##PARAMETER ESTIMATE
weib=pelwei(nso$Vm) #parameter estimate for Weibull
ks.test(nso$Vm, "pweibull", shape=weib["delta"], scale=weib["beta"])

gma=pelgam(nso$Vm) # parameter estimate for gamma
ks.test(nso$Vm, "pgamma", shape=gma["alpha"], rate=gma["beta"])

lgs=pelln3(nso$Vm)
ks.test(nso$Vm, "plnorm")

exp=pelexp(nso$Vm)
ks.test(nso$Vm, "pexp", rate=exp["rate"])

x.wei<-rweibull(n=length(nso$Vm), shape=moments["delta"], scale=moments["beta"])    
hist(as.numeric(nso$Vm), freq=FALSE)    
lines(density(x.wei), col="red", lwd=4)  

##plotting everything on a lm
lm1=lm(sq~cond*bin, data=nso)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm1)
plot(lm1, id.n = NULL)               # no id's
plot(lm1, id.n = 5, labels.id = NULL)# 5 id numbers
