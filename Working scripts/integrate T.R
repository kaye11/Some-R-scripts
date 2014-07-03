library(ggplot2)
library(fmsb)
library(data.table)
library(plyr)
library(nlme)

##time integrated
aloxsum<- ddply(alox, .(A, bin, cond), summarise, meanvm = mean(Vm, na.rm = TRUE), 
                 meanfreq = mean(freq, na.rm = TRUE), meandir=mean(dir, na.rm=TRUE), meanvs=mean(sqrt(Vm)))

str(aloxsum)
qplot(bin, meanvs, data = aloxsum, geom = "boxplot", fill = cond) + theme_classic()

aloxsum$binn=as.numeric(aloxsum$bin)

#model validation
op=par(mfrow=c(2,2), mar=c(5,4,2,2))
e=resid(m.gls2)
plot(m.gls2, add.smooth=FALSE, which=1) #resid vs. fitted
hist(e, xlab="Residuals", main="")
plot(aloxsum$cond, resid(m.gls2), xlab="Mean Velocity", ylab="Residuals")
plot(aloxsum$bin, resid(m.gls2), xlab="Mean Velocity", ylab="Residuals")
par(op)

#linear regression model
m.lm=lm(meanvs~cond*binn, data=aloxsum)
m.gls0= gls(meanvs~cond*binn, data=aloxsum, method="REML")

vf1fixed=varFixed(~binn)
vf1fixed2=varFixed(~A)
m.gls1= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf1fixed)
m.gls11= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf1fixed2)

vf2=varIdent(form=~1|binn)
vf21=varIdent(form=~1|A)
vf22=varIdent(form=~1|binn*factor(A))
m.gls2= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf2)
m.gls21= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf21)
m.gls22= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf22)


vf3=varPower(form=~binn)
vf31=varPower(form=~binn)
vf32=varPower(form=~binn|A)
m.gls3= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf3)
m.gls31= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf31)
m.gls32= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf32)


vf4=varExp(form=~binn)
vf41=varExp(form=~A)
vf42=varExp(form=~binn|A)
m.gls4= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf4)
m.gls41= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf41)
m.gls42= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf42)

vf5=varConstPower(form=~binn)
vf51=varConstPower(form=~A)
vf52=varConstPower(form=~binn|A)
m.gls5= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf5)
m.gls51= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf51)
m.gls52= gls(meanvs~cond*binn, data=aloxsum, method="REML", weights=vf52)

anova(m.gls0, m.gls1, m.gls2, m.gls3, m.gls4, m.gls5)
plot(ACF(m.gls2),alpha=0.05)
plot(ACF(m.gls2,resType="normalized"),alpha=0.05)

##m.gls2 showed the lowest AIC

e1=resid(m.gls2)
coplot(e1~cond|bin, ylab="Ordinary Residuals", data=aloxsum)

e2=resid(m.gls2, type="normalized")
coplot(e2~cond|bin, ylab="Ordinary Residuals", data=aloxsum)


plot(m.gls2, col=1) #plots residuals vs. fitted

##backwards selection1
vfoptim=varIdent(form=~1|binn)

ffull= meanvs~cond+binn+cond:binn

m2full=gls(ffull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:binn
m2.drop1=update(m2full, .~. -cond:binn)
anova(m2full, m2.drop1)
#drop cond
m2.drop2=update(m2full, .~. -cond)
anova(m2full, m2.drop2)
#drop binn
m2.drop3=update(m2full, .~. -binn)
anova(m2full, m2.drop3)

#verdict: drop bin

##backwards selection2
nfull= meanvs~cond+cond:binn

m2nfull=gls(nfull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:binn
m2n.drop1=update(m2nfull, .~. -cond:binn)
anova(m2nfull, m2n.drop1)
#drop cond
m2n.drop2=update(m2nfull, .~. -cond)
anova(m2nfull, m2n.drop2)

##final model
mfinal=gls(meanvs~cond+cond:binn, weights=vfoptim, method="REML", data=aloxsum)
e=resid(mfinal, type="normalized")
fit=fitted(mfinal)
op=par(mfrow=c(1,2))
plot(x=fit, y=e, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
hist(e, nclass=15, main="Histogram of residuals")

identify(fit, e)
par(op)

##correlation
acf(e)
pacf(e)

m.lme=lme(meanvs~cond*binn, random=~1|binn, data=aloxsum)
m.lme2=lme(meanvs~cond*binn, random=~1|A, data=aloxsum)
anova(m.lme, m.lme2, mfinal, m.lme3)
m.lme3=lme(meanvs~cond/binn, random=~1|A, data=aloxsum)

