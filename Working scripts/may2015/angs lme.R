library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(nlme)

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
source("summarySE.R")

tm=seq(0, 600, by = 60)
binned$time2 <- cut(binned$T, tm, labels=paste(tail(tm, -1L)))
binned$time2[is.na(binned$time2)] <- 60 #replace NAs with 30. some data time points have the starting point as NA
binned$time2=as.numeric(binned$time2)*60
binned$cond=as.factor(binned$cond)

binAdata=subset(binned, binned$bin=="binA")
binBdata=subset(binned, binned$bin=="binB")
binCdata=subset(binned, binned$bin=="binC")
binAdata$timef=as.factor(binAdata$time2)
binBdata$timef=as.factor(binBdata$time2)
binCdata$timef=as.factor(binCdata$time2)


#Bin A
binAsum <- summarySE(binAdata, measurevar="angs", groupvars=c("ID", "cond", "time2"), na.rm=TRUE)

expA=as.data.frame(data.table(cbind(cond=binAsum$cond, T=binAsum$time2, ID=binAsum$ID)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#normality
source("shapirobig.R")
sf.testBIG(binAdata$angs) #not normal

#levene
library(lawstat)
levene.test(binAdata$angs, group=binAdata$ID, location="mean") #equal
levene.test(binAdata$angs, group=binAdata$time2, location="mean") # equal

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, binAdata)
boxplot(angs~ID, binAdata)
boxplot (angs~time2, binAdata)

#lm model
BinA.lm <- lm(angs ~cond*time2, binAdata)

#barplots again
op=par(mfrow=c(2,2))
plot(BinA.lm)


#fit a gls
Form <- formula (angs ~ cond*time2)
BinA.gls<- gls(Form, binAdata, na.action=na.omit)


#nlme model
BinA1.lme <- lme (Form, random = ~1|ID, method="REML", binAdata, na.action=na.omit)

#BinA2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", binAdata, na.action=na.omit)

#BinA3.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), 
                  correlation=corAR1 (form=~1|ID/cond), method="REML", binAdata, na.action=na.omit)

#BinA4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|time2), 
                  correlation=corAR1 (form=~1|ID/cond), method="REML", binAdata, na.action=na.omit) 

anova(BinA.gls, BinA1.lme)

summary(BinA1.lme)
anova(BinA1.lme)

#residuals
BinA.E2<-resid(BinA1.lme,type="normalized")
BinA.F2<-fitted(BinA1.lme)
MyYlab="Residuals"

plot(x=BinA.F2,y=BinA.E2,xlab="Fitted values",ylab=MyYlab)



#Bin B
binBsum <- summarySE(binBdata, measurevar="angs", groupvars=c("ID", "cond", "time2"), na.rm=TRUE)

expB=as.data.frame(data.table(cbind(cond=binBsum$cond, T=binBsum$time2, ID=binBsum$ID)))
cor(expB, method = "spearman")


vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#normality
source("shapirobig.R")
sf.testBIG(binBdata$angs) #not normal

#levene
library(lawstat)
levene.test(binBdata$angs, group=binBdata$ID, location="mean") #unequal
levene.test(binBdata$angs, group=binBdata$time2, location="mean") # equal

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, binBdata)
boxplot(angs~ID, binBdata)
boxplot (angs~time2, binBdata)

#lm model
binB.lm <- lm(angs ~cond*time2, binBdata)

#barplots again
op=par(mfrow=c(2,2))
plot(binB.lm)


#fit a gls
Form <- formula (angs ~ cond*time2)
binB.gls<- gls(Form, binBdata, na.action=na.omit)


#nlme model
binB1.lme <- lme (Form, random = ~1|ID, method="REML", binBdata, na.action=na.omit)

#binB2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", binBdata, na.action=na.omit)

#binB3.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), correlation=corAR1 (form=~1|ID/cond), method="REML", binBdata, na.action=na.omit)

#binB4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|time2), correlation=corAR1 (form=~1|ID/cond), method="REML", binBdata, na.action=na.omit) 

anova(binB.gls, binB1.lme)

summary(binB1.lme)
anova(binB1.lme)

#residuals
binB.E2<-resid(binB1.lme,type="normalized")
binB.F2<-fitted(binB1.lme)
MyYlab="Residuals"

plot(x=binB.F2,y=binB.E2,xlab="Fitted values",ylab=MyYlab)

plot(binB1.lme)


#Bin C

#Bin C
binCsum <- summarySE(binCdata, measurevar="angs", groupvars=c("ID", "cond", "time2"), na.rm=TRUE)

expC=as.data.frame(data.table(cbind(cond=binCsum$cond, T=binCsum$time2, ID=binCsum$ID)))
cor(expC, method = "spearman")


vif_func(in_frame=expC,thresh=5,trace=T)

pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#normality
source("shapirobig.R")
sf.testBIG(binCdata$angs) #not normal

#levene
library(lawstat)
levene.test(binCdata$angs, group=binCdata$ID, location="mean") #equal
levene.test(binCdata$angs, group=binCdata$time2, location="mean") # equal

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, binCdata)
boxplot(angs~ID, binCdata)
boxplot (angs~time2, binCdata)

#lm model
binC.lm <- lm(angs ~cond*time2, binCdata)

#barplots again
op=par(mfrow=c(2,2))
plot(binC.lm)


#fit a gls
Form <- formula (angs ~ cond*time2)
binC.gls<- gls(Form, binCdata, na.action=na.omit)


#nlme model
binC1.lme <- lme (Form, random = ~1|ID, method="REML", binCdata, na.action=na.omit)

#binC2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", binCdata, na.action=na.omit)

#binC3.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), correlation=corAR1 (form=~1|ID/cond), method="REML", binCdata, na.action=na.omit)

#binC4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|time2), correlation=corAR1 (form=~1|ID/cond), method="REML", binCdata, na.action=na.omit) 

anova(binC.gls, binC1.lme)

summary(binC1.lme)
anova(binC1.lme)

#residuals
binC.E2<-resid(binC1.lme,type="normalized")
binC.F2<-fitted(binC1.lme)
MyYlab="Residuals"

plot(x=binC.F2,y=binC.E2,xlab="Fitted values",ylab=MyYlab)

plot(binC1.lme)


