library(fBasics)
library(vcd)

x.norm<-rnorm(n=200,m=10,sd=2) 

hist(nso$Vm,main="Histogram of observed data") #getting a histogram
plot(density(nso$Vm),main="Density estimate of data") #getting a density plot
plot(ecdf(nso$Vm),main= "Empirical cumulative distribution function")

z.norm<-(nso$Vm-mean(nso$Vm))/sd(nso$Vm) ## standardized data 
qqnorm(z.norm) ## drawing the QQplot 
abline(0,1) ## drawing a 45-degree reference line 

##A 45-degree reference line is also plotted. If the empirical data come from the 
#population with the choosen distribution, the points should fall approximately along this 
#reference line. The greater the departure from this reference line, the greater the 
#evidence for the conclusion that the data set have come from a population 
#with a different distribution

x.wei<-rweibull(n=200,shape=2.1,scale=1.1) ## sampling from a Weibull distribution with parameters shape=2.1 and scale=1.1 
x.teo<-rweibull(n=200,shape=2, scale=1) ## theorical quantiles from a Weibull population with known paramters shape=2 e scale=1 
qqplot(x.teo,x.wei,main="QQ-plot distr. Weibull") ## QQ-plot 
abline(0,1) ## a 45-degree reference line is plotted 

x.poi<-rpois(n=200,lambda=2.5) 
hist(x.poi,main="Poisson distribution") 

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


x.gam<-rgamma(200,rate=0.5,shape=3.5) ## sampling from a gamma distribution with 
#l=0.5 (scale parameter 12) and a=3.5 (shape parameter) 

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

rate <- mean(nso$Vm)/var(nso$Vm)



##chi square continous
## computing relative expected frequencies 
p<-c((pgamma(3,shape=3.5,rate=0.5)-pgamma(0,shape=3.5,rate=0.5)), 
     (pgamma(6,shape=3.5,rate=0.5)-pgamma(3,shape=3.5,rate=0.5)), 
     (pgamma(9,shape=3.5,rate=0.5)-pgamma(6,shape=3.5,rate=0.5)), 
     (pgamma(12,shape=3.5,rate=0.5)-pgamma(9,shape=3.5,rate=0.5)), 
     (pgamma(18,shape=3.5,rate=0.5)-pgamma(12,shape=3.5,rate=0.5))) 

chisq.test(x=f.os,p=p) ## chi-square test 

ks.test(x.wei,"pweibull", shape=2,scale=1) 

x<-seq(0,2,0.1) 
plot(x,pweibull(x,scale=1,shape=2),type="l",col="red", main="ECDF and 
     Weibull CDF") 
plot(ecdf(x.wei),add=TRUE) 




