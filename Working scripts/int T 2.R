library(ggplot2)
library(fmsb)
library(data.table)
library(plyr)
library(nlme)

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
vf1fixed3=varFixed(~A)
m.gls1= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf1fixed)
m.gls11=gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf1fixed2)
m.gls12=gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf1fixed3)


vf2=varIdent(form=~1|time)
vf21=varIdent(form=~1|binn)
vf22=varIdent(form=~1|A)
vf23=varIdent(form=~1|time*factor(binn))
vf24=varIdent(form=~1|time*factor(A))
m.gls2= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf2)
m.gls21= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf21)
m.gls22= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf22) # did not converge
m.gls23= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf23)
m.gls24= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf24) # did not converge



vf3=varPower(form=~binn)
vf31=varPower(form=~time)
vf32=varPower(form=~A)
vf33=varPower(form=~binn|time)
vf34=varPower(form=~binn|A)
vf35=varPower(form=~time|A)
m.gls3= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf3)
m.gls31= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf31)
m.gls32= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf32)
m.gls33= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf33)
m.gls34= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf34) # did not converge
m.gls35= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf35) # did not converge



vf4=varExp(form=~binn)
vf41=varExp(form=~time)
vf42=varExp(form=~A)
vf43=varExp(form=~binn|time)
vf44=varExp(form=~binn|A)
vf45=varExp(form=~time|A)
m.gls4= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf4)
m.gls41= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf41)
m.gls42= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf42)
m.gls43= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf43)
m.gls44= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf44) # did not converge
m.gls45= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf45) # did not converge

vf5=varConstPower(form=~binn)
vf51=varConstPower(form=~time)
vf51=varConstPower(form=~A)
vf52=varConstPower(form=~binn|time)
vf53=varConstPower(form=~binn|A)
vf54=varConstPower(form=~time|A)
m.gls5= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf5)
m.gls51= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf51)
m.gls52= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf52) # did not converge
m.gls53= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf53) # did not converge
m.gls54= gls(meanvs~cond*binn*time, data=aloxsum, method="REML", weights=vf54) # did not converge

anova(m.gls0, m.gls1, m.gls11, m.gls12, m.gls2, m.gls21,  m.gls23, m.gls3, 
      m.gls31, m.gls32, m.gls33,m.gls4, m.gls41, m.gls42, m.gls43, 
      m.gls5, m.gls51)

#m.gls43 best model fit

plot(ACF(m.gls43),alpha=0.05)
plot(ACF(m.gls43,resType="normalized"),alpha=0.05)

#model validation
op=par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(m.gls43, add.smooth=FALSE, which=1) #resid vs. fitted
e=res(m.gls43)
hist(e, xlab="Residuals", main="") 
plot(aloxsum$cond, resid(m.gls43), xlab="Mean Velocity", ylab="Residuals")
plot(aloxsum$bin, resid(m.gls43), xlab="Mean Velocity", ylab="Residuals")
plot(aloxsum$time, resid(m.gls43), xlab="Mean Velocity", ylab="Residuals")

par(op)

plot(m.gls43, col=1) #plots residuals vs. fitted

##backwards selection 1
vfoptim=varExp(form=~binn|time)
ffull=meanvs~cond+binn+time+ cond:binn + cond:time + binn:time + cond:binn:time

m43full=gls(ffull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:binn
m43.drop1=update(m43full, .~. -cond:binn)
anova(m43full, m43.drop1)
#drop cond:time
m43.drop2=update(m43full, .~. -cond:time)
anova(m43full, m43.drop2)
#drop binn:time
m43.drop3=update(m43full, .~. -binn:time)
anova(m43full, m43.drop3)
#drop cond:binn:time
m43.drop4=update(m43full, .~. -cond:binn:time) #fakse convergence
anova(m43full, m43.drop4)

#verdict:drop cond:binn and binn:time
##backwards selection2
nfull=meanvs~cond+binn+time+ cond:time + cond:binn:time

m43nfull=gls(ffull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:time
m43n.drop2=update(m43nfull, .~. -cond:time)
anova(m43nfull, m43n.drop2)
#drop cond:binn:time
m43n.drop4=update(m43nfull, .~. -cond:binn:time) #false convergence
anova(m43nfull, m43n.drop4)
#drop cond
m43n.drop5=update(m43nfull, .~. -cond)
anova(m43nfull, m43n.drop5)
#drop binn
m43n.drop6=update(m43nfull, .~. -binn) # false convergence
anova(m43nfull, m43n.drop6)
#drop time
m43n.drop7=update(m43nfull, .~. -time)
anova(m43nfull, m43n.drop7)

#verdict:drop time
##backwards selection3
pfull=meanvs~cond + binn + cond:time + cond:binn:time

m43pfull=gls(pfull, weights=vfoptim, method="ML", data=aloxsum)

#drop cond:time
m43p.drop2=update(m43pfull, .~. -cond:time)
anova(m43pfull, m43p.drop2)
#drop cond:binn:time
m43p.drop4=update(m43pfull, .~. -cond:binn:time) # false convergence
anova(m43pfull, m43p.drop4)
#drop cond
m43p.drop5=update(m43pfull, .~. -cond) # false convergence
anova(m43pfull, m43p.drop5)

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
anova(m.lme, m.lme2, m.lme3, mfinal, m.lme4)

m.lme4=lme(meanvs~cond*binn*time, random=~1+binn|A, data=aloxsum)

m.lme5=lme(meanvs~cond*binn*time, random=~1+time|A, data=aloxsum)

m.lme6=lme(meanvs~1+time*cond*binn, random=~1+time|A, data=aloxsum)

m.lme7=lme(meanvs~1+time*A*cond*binn, random=~1+time|A, data=aloxsum)


m.lme5=lme(meanvs~cond*binn*time, random=~1+time|A, data=aloxsum) #best model

m.lme6=lme(meanvs~1+time*cond*binn, random=~1+time|A, data=aloxsum)

e.lme5=resid(m.lme5)
acf(e.lme)

m.lme7=lme(meanvs~1+time*cond*binn, random=~1+time|binn/A, data=aloxsum)
m.lme8=lme(meanvs~1+time*cond*binn, random=~1+binn|time/A, data=aloxsum)


