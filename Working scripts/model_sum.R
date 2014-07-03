library(ggplot2)
library(fmsb)
library(data.table)
library(plyr)
library(car)
library(lmtest)

alox=alox2
aloxsum <- ddply(alox, .(A, bin, cond, time), summarise, meanvm = mean(Vm, na.rm = TRUE), 
                 meanfreq = mean(freq, na.rm = TRUE), meandir=mean(dir, na.rm=TRUE), meanvs=mean(sqrt(Vm)))

aloxsum$T <- ts(aloxsum$time) #coerce time to become a ts object
str(aloxsum)
qplot(bin, meanvs, data = aloxsum, geom = "boxplot", fill = cond) + theme_classic()

##transform categorical variables to numeric character
aloxsum$condn=as.numeric(aloxsum$cond)
aloxsum$binn=as.numeric(aloxsum$bin)

##I did not include how I found out the whole best fit model, because it will be a very long code.
#I followed Zuur's step by step process on both gls and lme 


#finalmodel gls
vf43=varExp(form=~binn|time)
m.gls=gls(meanvs~cond*binn*time, method="REML", weights=vf43, data=aloxsum)

#lme
m5=lme(meanvs~cond:time+cond:binn+cond:binn:time, random=~1+time|binn/A, method="REML", data=aloxsum)

#lme check
e.lm=resid(m5, type="normalized")
fit.lm=fitted(m5)
op=par(mfrow=c(1,2))
plot(x=fit.lm, y=e.lm, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
hist(e.lm, nclass=15)
acf(e.lm)
pacf(e.lm)

#gls check
e.gls=resid(m.gls, type="normalized")
fit.gls=fitted(m.gls)
op=par(mfrow=c(1,2))
plot(x=fit.gls, y=e.gls, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
hist(e.gls, nclass=15)
acf(e.gls)
pacf(e.gls)

bptest(meanvs~cond:time+cond:binn+cond:binn:time, data=aloxsum) #check heteroscedasticity

m5.1=rlm(meanvs~cond:time+cond:binn+cond:binn:time, random=~1+time|binn/A, method="REML", data=aloxsum)

bartlett.test(e.lm, aloxsum$time)


