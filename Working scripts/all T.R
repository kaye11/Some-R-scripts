library(ggplot2)
library(fmsb)
library(data.table)
library(plyr)

aloxsum <- ddply(alox, .(A, bin, cond, time), summarise, meanvm = mean(Vm, na.rm = TRUE), 
                 meanfreq = mean(freq, na.rm = TRUE), meandir=mean(dir, na.rm=TRUE), meanvs=mean(sqrt(Vm)))

aloxsum$T <- ts(aloxsum$time) #coerce time to become a ts object
str(aloxsum)
qplot(bin, meanvs, data = aloxsum, geom = "boxplot", fill = cond) + theme_classic()

##transform categorical variables to numeric character
aloxsum$condn=as.numeric(aloxsum$cond)
aloxsum$binn=as.numeric(aloxsum$bin)

#linear regression model
m.lm=lm(meanvs~cond*binn*time, data=aloxsum)
m.gls0= gls(meanvs~cond*binn*time, data=aloxsum, method="REML")

vf1fixed=varFixed(~binn)
vf1fixed2=varFixed(~time)
m.gls1= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf1fixed)
m.gls11=gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf1fixed2)

vf2=varIdent(form=~1|time)
vf21=varIdent(form=~1|binn)
vf22=varIdent(form=~1|time*factor(binn))
m.gls2= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf2)
m.gls21= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf21)
m.gls22= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf22)

vf3=varPower(form=~binn)
vf31=varPower(form=~time)
vf32=varPower(form=~binn|time)
m.gls3= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf3)
m.gls31= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf31)
m.gls32= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf32)

vf4=varExp(form=~binn)
vf41=varExp(form=~time)
vf42=varExp(form=~binn|time)
m.gls4= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf4)
m.gls41= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf41)
m.gls42= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf42)

vf5=varConstPower(form=~binn)
vf51=varConstPower(form=~time)
vf52=varConstPower(form=~binn|time)
m.gls5= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf5)
m.gls51= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf51)
m.gls52= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf52) ##do not work,

anova(m.gls0, m.gls1, m.gls11, m.gls2, m.gls21, m.gls22, m.gls3, m.gls31, m.gls32, m.gls4, m.gls41, m.gls42, m.gls5, m.gls51)

#m.gls32 best model fit

plot(ACF(m.gls2),alpha=0.05)
plot(ACF(m.gls2,resType="normalized"),alpha=0.05)

#model validation
op=par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(m.gls32, add.smooth=FALSE, which=1) #resid vs. fitted
e=res(m.gls32)
hist(e, xlab="Residuals", main="") 
plot(aloxsum$cond, resid(m.gls32), xlab="Mean Velocity", ylab="Residuals")
plot(aloxsum$bin, resid(m.gls32), xlab="Mean Velocity", ylab="Residuals")
plot(aloxsum$time, resid(m.gls32), xlab="Mean Velocity", ylab="Residuals")

par(op)

plot(m.gls32, col=1) #plots residuals vs. fitted

##backwards selection 1
vfoptim=varPower(form=~binn|time)
ffull=meanvs~cond+binn+time+ cond:binn + cond:time + binn:time + cond:binn:time

m32full=gls(ffull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:binn
m32.drop1=update(m32full, .~. -cond:binn)
anova(m32full, m32.drop1)
#drop cond:time
m32.drop2=update(m32full, .~. -cond:time)
anova(m32full, m32.drop2)
#drop binn:time
m32.drop3=update(m32full, .~. -binn:time)
anova(m32full, m32.drop3)
#drop cond:binn:time
m32.drop4=update(m32full, .~. -cond:binn:time)
anova(m32full, m32.drop4)

#verdict:drop cond:binn and binn:time
##backwards selection2
nfull=meanvs~cond+binn+time+ cond:time + cond:binn:time

m32nfull=gls(ffull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:time
m32n.drop2=update(m32nfull, .~. -cond:time)
anova(m32nfull, m32n.drop2)
#drop cond:binn:time
m32n.drop4=update(m32nfull, .~. -cond:binn:time)
anova(m32nfull, m32n.drop4)
#drop cond
m32n.drop5=update(m32nfull, .~. -cond)
anova(m32nfull, m32n.drop5)
#drop binn
m32n.drop6=update(m32nfull, .~. -binn)
anova(m32nfull, m32n.drop6)
#drop time
m32n.drop7=update(m32nfull, .~. -time)
anova(m32nfull, m32n.drop7)

#verdict:drop binn and time
##backwards selection3
pfull=meanvs~cond+ cond:time + cond:binn:time

m32pfull=gls(pfull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:time
m32p.drop2=update(m32pfull, .~. -cond:time)
anova(m32pfull, m32p.drop2)
#drop cond:binn:time
m32p.drop4=update(m32pfull, .~. -cond:binn:time)
anova(m32pfull, m32p.drop4)
#drop cond
m32p.drop5=update(m32pfull, .~. -cond)
anova(m32pfull, m32p.drop5)

#finalmodel
mfinal=gls(meanvs~cond+ cond:time + cond:binn:time, weights=vfoptim, method="REML", data=aloxsum)
e=resid(mfinal, type="normalized")
fit=fitted(mfinal)
op=par(mfrow=c(1,2))
plot(x=fit, y=e, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
identify(fit, e)
hist(e, nclass=15)

par(op)

acf(e)
pacf(e)

m.lme=lme(meanvs~cond*binn*time, random=~1|time, data=aloxsum)
m.lme2=lme(meanvs~cond*binn*time, random=~1|binn, data=aloxsum)
m.lme3=lme(meanvs~cond*binn*time, random=~1|A, data=aloxsum)
anova(m.lme, m.lme2, m.lme3, mfinal)


##backwards selection
lmefull=meanvs~cond+binn+time+ cond:binn + cond:time + binn:time + cond:binn:time

lmef=lme(lmefull, random=~1|A, data=aloxsum, method="ML")

#drop cond:binn
lmef.drop1=update(lmef, .~. -cond:binn)
anova(lmef, lmef.drop1)
#drop cond:time
lmef.drop2=update(lmef, .~. -cond:time)
anova(lmef, lmef.drop2)
#drop binn:time
lmef.drop3=update(lmef, .~. -binn:time)
anova(lmef, lmef.drop3)
#drop cond:binn:time
lmef.drop4=update(lmef, .~. -cond:binn:time)
anova(lmef, lmef.drop4)
#drop cond
lmef.drop5=update(lmef, .~. -cond)
anova(lmef, lmef.drop5)
#drop binn
lmef.drop6=update(lmef, .~. -binn)
anova(lmef, lmef.drop6)
#drop time
lmef.drop7=update(lmef, .~. -time)
anova(lmef, lmef.drop7)

##drop binn:time, binn and time
lmefull2=meanvs~cond+ cond:binn + cond:time + cond:binn:time

mlmef=lme(lmefull2, random=~1|A, data=aloxsum, method="ML")

#drop cond:binn
mlmef.drop1=update(mlmef, .~. -cond:binn)
anova(mlmef, mlmef.drop1)
#drop cond:time
mlmef.drop2=update(mlmef, .~. -cond:time)
anova(mlmef, mlmef.drop2)
#drop cond:binn:time
mlmef.drop4=update(mlmef, .~. -cond:binn:time)
anova(mlmef, mlmef.drop4)
#drop cond
mlmef.drop5=update(mlmef, .~. -cond)
anova(mlmef, mlmef.drop5)

lmefinal=lme(meanvs~cond+ cond:binn + cond:time + cond:binn:time, random=~1|A, data=aloxsum)
plot(lmefinal, col=1) #plots residuals vs. fitted
