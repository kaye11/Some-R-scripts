m.lme5=lme(meanvs~cond*binn*time, random=~1+time|A, data=aloxsum) #best model

#step 1 linear regression
m.lm=lm(meanvs~cond*binn*time, data=aloxsum)
op=par(mfrow=c(2,2))
plot(m.lm, select=c(1))
par(op)

#step 2 gls
form=formula(meanvs~cond*binn*time)
m.gls=gls(form, data=aloxsum)

#step 3 gls variance structure, skipped

#step 4 lme
m.lme=lme(form, random=~1+time|A, data=aloxsum) #best model

#step 5, compare models
anova(m.gls, m.lme)

#step6 everything ok?
E2 <- resid(m.lme, type = "normalized")
F2 <- fitted(m.lme)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
MyYlab <- "Residuals"
plot(x = F2, y = E2, xlab = "Fitted values", ylab = MyYlab)
boxplot(E2 ~ cond, data = aloxsum,
          main = "Cond", ylab = MyYlab)
boxplot(E2 ~ bin, data = aloxsum,
        main = "Bin", ylab = MyYlab)
E=rstandard(m.lm)
plot(x = aloxsum$time, y = E, ylab = MyYlab,
       main = "Time", xlab = "Time (sec)")

#step7 and 8 optimal fixed structure
summary(m.lme) #binn, time, condSi, binn*time could be dropped

m1.full=lme(form, random=~1+time|A, method="ML", data=aloxsum)
m1.a=update(m1.full, .~. -binn)
m1.b=update(m1.full, .~. -time)
m1.c=update(m1.full, .~. -condSi)
m1.d=update(m1.full, .~. -binn:time)

anova(m1.full, m1.a)
anova(m1.full, m1.b)
anova(m1.full, m1.c)
anova(m1.full, m1.d)
#drop all in updates

form2=formula(meanvs~cond+cond:time+cond:binn+cond:binn:time)
m2.full=lme(form2, random=~1+time|A, method="ML", data=aloxsum)
m2.a=update(m2.full, .~. -cond)
m2.b=update(m2.full, .~. -cond:time)
m2.c=update(m2.full, .~. -cond:binn)
m2.d=update(m2.full, .~. -cond:time:binn)

anova(m2.full, m2.a)
anova(m2.full, m2.b)
anova(m2.full, m2.c)
anova(m2.full, m2.d)
#drop cond

form3=formula(meanvs~cond:time+cond:binn+cond:binn:time)
m3.full=lme(form3, random=~1+time|A, method="ML", data=aloxsum)
m3.a=update(m3.full, .~. -cond:time)
m3.b=update(m3.full, .~. -cond:binn)
m3.c=update(m3.full, .~. -cond:time:binn)

anova(m3.full, m3.a)
anova(m3.full, m3.b)
anova(m3.full, m3.c)

m4=lme(meanvs~cond:time+cond:binn+cond:binn:time, random=~1+time|A, method="REML", data=aloxsum)

xyplot(E2 ~ time | cond * bin,
       data = aloxsum, ylab = "Residuals",
       xlab = "time",
       panel = function(x,y){
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, span = 0.5, col = 1,lwd=2)})

m.gamm=gamm(meanvs~cond*bin*time, random=list(time=~1+time|A), data=aloxsum)

E3 <- resid(m5, type = "normalized")
F3 <- fitted(m5)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
MyYlab <- "Residuals"
plot(x = F3, y = E3, xlab = "Fitted values", ylab = MyYlab)
boxplot(E3 ~ cond, data = aloxsum,
        main = "Cond", ylab = MyYlab)
boxplot(E3 ~ bin, data = aloxsum,
        main = "Bin", ylab = MyYlab)
E=rstandard(m.lm)
plot(x = aloxsum$time, y = E, ylab = MyYlab,
     main = "Time", xlab = "Time (sec)")

m5=lme(meanvs~cond:time+cond:bin+cond:bin:time, random=~1+time|bin/A, method="REML", data=aloxsum)
m6=lme(meanvs~cond:time+cond:binn+cond:binn:time, random=~1+binn|time/A, method="REML", data=aloxsum) #no convergence
m5.1=lme(meanvs~cond*binn*time, random=~1+time|binn/A, method="REML", data=aloxsum)

##with time
#test random as nested, did not work  
lme.t1=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time<61, ]) #best model
vf43=varExp(form=~binn|time)
m.gls=gls(meanvs~cond*binn*time, method="ML", weights=vf43, data=aloxsum)

lme.t2=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>60 & aloxsum$time<121, ]) #best model
lme.t3=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>120 & aloxsum$time<181, ]) #best model
lme.t4=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>180 & aloxsum$time<241, ]) #best model
lme.t5=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>240 & aloxsum$time<301, ]) #best model
lme.t6=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>300 & aloxsum$time<361, ]) #best model
lme.t7=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>360 & aloxsum$time<421, ]) #best model
lme.t8=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>420 & aloxsum$time<481, ]) #best model
lme.t9=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>480 & aloxsum$time<541, ]) #best model
lme.t10=lme(meanvs~cond*binn*time, random=~1+time|A, method="ML", data=aloxsum [aloxsum$time>541 & aloxsum$time<601, ]) #best model


#finalmodel
e.lm=resid(m5, type="normalized")
fit.lm=fitted(m5)
op=par(mfrow=c(1,2))
plot(x=fit.lm, y=e.lm, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
hist(e.lm, nclass=15)

#finalmodel
e.gls=resid(m.gls, type="normalized")
fit.gls=fitted(m.gls)
op=par(mfrow=c(1,2))
plot(x=fit.gls, y=e.gls, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
hist(e.gls, nclass=15)

anova
m7=lme(meanvs~cond*bin*time, random=~time|bin/A, method="REML", data=aloxsum, 
       correlation = corCompSymm(form=~time|bin/A))


m6=lme(meanvs~cond*bin*time, random=~time|A, method="REML", data=aloxsum, 
       correlation = corCompSymm(form=~time|A))

anova(m6)
e.lm=resid(m6, type="normalized")
fit.lm=fitted(m6)
op=par(mfrow=c(1,2))
plot(x=fit.lm, y=e.lm, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
hist(e.lm, nclass=15)
acf(e.lm)
pacf(e.lm)
summary(m6)

anova(m7)
e.lm=resid(m7, type="normalized")
fit.lm=fitted(m7)
op=par(mfrow=c(1,2))
plot(x=fit.lm, y=e.lm, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted values")
hist(e.lm, nclass=15)
acf(e.lm)
pacf(e.lm)
summary(m7)

m7=lme(meanvs~cond*bin*time, random=~time|bin/A, method="ML", data=aloxsum, 
       correlation = corCompSymm(form=~time|bin/A))

m7=lme(meanvs~cond*bin*time, random=~time|bin/A, method="ML", data=aloxsum, 
       correlation = corCompSymm(form=~time|bin/A))

aloxplot=aloxsum[-c(9:11)]
plot(aloxplot)
cor(aloxplot)

-correlation