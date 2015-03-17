library(fBasics)
library(vcd)

#normal
x.norm<-rnorm(n=binAdata$Vlog,m=0.80,sd=0.71) 
hist(binAdata$Vlog,main="Histogram of observed data") #getting a histogram
plot(density(binAdata$Vlog),main="Density estimate of data") #getting a density plot
plot(ecdf(binAdata$Vlog),main= "Empirical cumulative distribution function")
qqplot(binAdata$Vlog, x.norm)
abline(0,1)

z.norm<-(binAdata$Vlog-mean(binAdata$Vlog))/sd(binAdata$Vlog) ## standardized data 
qqnorm(z.norm) ## drawing the QQplot 
abline(0,1) ## drawing a 45-degree reference line 

##A 45-degree reference line is also plotted. If the empirical data come from the 
#population with the choosen distribution, the points should fall approximately along this 
#reference line. The greater the departure from this reference line, the greater the 
#evidence for the conclusion that the data set have come from a population 
#with a different distribution


x.poi<-rpois(binAdata$Vlog,lambda=mean(binAdata$Vlog)) 
hist(x.poi,main="Poisson distribution") 
plot(density(x.poi))
qqplot(binAdata$Vlog, x.poi)
abline(0,1)

x.gam<-rgamma(binAdata$Vlog,scale=0.76,shape=12.95) 
hist(x.gam)
plot(density(x.gam))
qqplot(binAdata$Vlog, x.gam)
abline(0,1)


x.wei<-rweibull(binAdata$Vlog,shape=3.31,scale=0.81) 
hist(x.wei)
plot(density(x.wei))
#x.teo<-rweibull(binAdata$Vlog,shape=2, scale=1) ## theorical quantiles from a Weibull population with known paramters shape=2 e scale=1 
qqplot(binAdata$Vlog, x.wei)
abline(0,1)

x.log <- rlogis(binAdata$Vlog, location=5.05, scale=2)
hist(x.log)
plot(density(x.log))

#qqplot(x.teo,x.wei,main="QQ-plot distr. Weibull") ## QQ-plot 
#abline(0,1) ## a 45-degree reference line is plotted 

fitdistr(x.gam,"gamma") ## fitting gamma pdf parameters 

fitdistr(x.wei,"weibull")

fitdistr(x.norm,"normal") ## fitting gaussian pdf parameters

fitdistr(x.log, "logistic")

ks.test(x.gam, "pgamma", scale=0.76, shape=12.95)

ks.test (x.wei, "pweibull", scale=0.81, shape=3.31)

ks.test(x.norm, "pnorm", m=0.80, sd= 0.72)

ks.test(x.log, "plogis", location=5.05, scale=2)

shapiro.test(x.norm)
shapiro.test(x.gam)
shapiro.test(x.wei)


curve(dnorm(x,m=10,sd=2),from=0,to=20,main="Normal distribution") 

curve(dgamma(x, scale=1.5, shape=2),from=0, to=15, main="Gamma distribution") 

curve(dweibull(x, scale=2.5, shape=1.5),from=0, to=15, main="Weibull distribution") 

library(fBasics) ## package loading 
skewness(x.norm) ## skewness of a normal distribution 
kurtosis(x.norm) ## kurtosis of a normal distribution 

skewness(x.wei) ## skewness of a Weibull distribution 
kurtosis(x.wei) ## kurtosis of a Weibull distribution 

mean.hat<-mean(x.norm) 
mean.hat 




med.gam<-mean(x.gam) ## sample mean 
var.gam<-var(x.gam) ## sample variance 
l.est<-med.gam/var.gam ## lambda estimate (corresponds to rate) 
a.est<-((med.gam)^2)/var.gam ## alfa estimate 

l.est 
a.est 

library(MASS) ## loading package MASS 

fitdistr(x.gam,"gamma") ## fitting gamma pdf parameters 

fitdistr(x.wei,densfun=dweibull,start=list(scale=1,shape=2))## fitting Weibull pdf parameters 

fitdistr(x.norm,"normal") ## fitting gaussian pdf parameters 

lambda.est<-mean(x.poi) ## estimate of parameter lambda 
tab.os<-table(x.poi)## table with empirical frequencies 
tab.os 
x.poi 


freq.os<-vector() 
for(i in 1: length(tab.os)) freq.os[i]<-tab.os[[i]] ## vector of emprical frequencies 
freq.ex<-(dpois(0:max(x.poi),lambda=lambda.est)*200) ## vector of fitted (expected) frequencies 
freq.os 

acc<-mean(abs(freq.os-trunc(freq.ex))) ## absolute goodness of fit index 
acc 

acc/mean(freq.os)*100 ## relative (percent) goodness of fit index 

h<-hist(x.norm,breaks=15) 
xhist<-c(min(h$breaks),h$breaks) 
yhist<-c(0,h$density,0) 
xfit<-seq(min(x.norm),max(x.norm),length=40) 
yfit<-dnorm(xfit,mean=mean(x.norm),sd=sd(x.norm)) 
plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)), main="Normal pdf and histogram")
lines(xfit,yfit, col="red") 

##GOODNESS OF FIT TESTS
library(vcd)## loading vcd package 

#for count data
gf<-goodfit(x.poi,type= "poisson",method= "MinChisq") 
summary(gf) 
plot(gf,main="Count data vs Poisson distribution") 

rate <- mean(binAdata$Vlog)/var(binAdata$Vlog)



##chi square continous
## computing relative expected frequencies 
p<-c((pgamma(3,shape=3.5,rate=0.5)-pgamma(0,shape=3.5,rate=0.5)), 
     (pgamma(6,shape=3.5,rate=0.5)-pgamma(3,shape=3.5,rate=0.5)), 
     (pgamma(9,shape=3.5,rate=0.5)-pgamma(6,shape=3.5,rate=0.5)), 
     (pgamma(12,shape=3.5,rate=0.5)-pgamma(9,shape=3.5,rate=0.5)), 
     (pgamma(18,shape=3.5,rate=0.5)-pgamma(12,shape=3.5,rate=0.5))) 

chisq.test(x=freq.os,p=p) ## chi-square test 

ks.test(x.wei,"pweibull", shape=2,scale=1) 

x<-seq(0,2,0.1) 
plot(x,pweibull(x,scale=1,shape=2),type="l",col="red", main="ECDF and 
     Weibull CDF") 
plot(ecdf(x.wei),add=TRUE) 




