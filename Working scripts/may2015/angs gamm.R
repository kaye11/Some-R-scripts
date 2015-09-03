library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(nlme)
s=mgcv:::s


##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")

#Bin A
BinA= subset (binned, bin=='binA')
binAsum <- summarySE(BinA, measurevar="angs", groupvars=c("ID", "cond", "T"), na.rm=TRUE)

expA=as.data.frame(data.table(cbind(cond=binAsum$cond, T=binAsum$T, ID=binAsum$ID)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, data=BinA)
boxplot(angs~ID, data=BinA)
boxplot (angs~T, data=BinA)

#levene
library(lawstat)
levene.test(BinA$angs, group=BinA$ID, location="mean") #equal
levene.test(BinA$angs, group=BinA$T, location="mean") # unequal


#gamm
BA <- gamm (angs~s(T, by=cond, bs="fs"), method="REML", data = BinA)
BA1 <- gamm (angs~s(T, by=cond, bs="fs", xt="cr"), method="REML", data = BinA) 
BA2 <- gamm (angs~s(T, by=cond, bs="fs", xt="cs"), method="REML", data = BinA) #best

anova(BA$lme, BA1$lme, BA2$lme)

#make random factor and correlations
fBinA <- angs~s(T, by=cond, bs="fs", xt="cs")

BA3 <- gamm (fBinA, method="REML",  random=list(ID=~1), data = BinA) 
BA4 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), data = BinA) #BEST
BA5 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (), data = BinA) #same with BA4

anova(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme)

#make variance structures
#BA6 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| T), data = BinA) #no convergence

BA7 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| ID), data = BinA) #no convergence

BA8 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), 
             weights = varIdent(form=~1| cond), data = BinA) #BEST






##BIN B

BinB= subset (binned, bin=='binB')
binBsum <- summarySE(BinB, measurevar="angs", groupvars=c("ID", "cond", "time2"), na.rm=TRUE)

expB=as.data.frame(data.table(cbind(cond=binBsum$cond, T=binBsum$time2, ID=binBsum$ID)))
cor(expB, method = "spearman")


vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, data=binBsum)
boxplot(angs~ID, data=binBsum)
boxplot (angs~time2, data=binBsum)

#lm model
BinB.lm <- lm(angs ~cond*time2, data=binBsum)

#barplots again
op=par(mfrow=c(2,2))
plot(BinB.lm)

BinB.E=rstandard(BinB.lm)
boxplot(BinB.E ~ cond, data=binBsum, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinB.E ~ time2, data=binBsum, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinB.E ~ ID, data=binBsum, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (angs ~ cond*time2)
BinB.gls<- gls(Form, data=binBsum)


#nlme model
BinB1.lme <- lme (Form, random = ~1|ID, method="REML", data=binBsum)

BinB2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", data=binBsum) #best

BinB3.lme <- lme (angs ~ cond*time2, random = ~1|ID,  weights=varIdent(form=~1|ID), 
                  correlation=corAR1 (form=~1|ID/cond), method="REML", data=binBsum) 

BinB4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|time2), 
                  correlation=corAR1 (form=~1|ID/cond), method="REML", data=binBsum) 

anova(BinB.gls, BinB1.lme, BinB2.lme, BinB3.lme, BinB4.lme)

summary(BinB2.lme)
anova(BinB2.lme)

#residuals
BinB.E2<-resid(BinB2.lme,type="normalized")
BinB.F2<-fitted(BinB2.lme)
op<-par(mfrow=c(1,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinB.F2,y=BinB.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinB.E2~cond,data=binBsum, main="cond",ylab=MyYlab)

##BIN C
BinC= subset (binned, bin=='binC')
binCsum <- summarySE(BinC, measurevar="angs", groupvars=c("ID", "cond", "time2"), na.rm=TRUE)

expC=as.data.frame(data.table(cbind(cond=binCsum$cond, T=binCsum$time2, ID=binCsum$ID)))
cor(expC, method = "spearman")


vif_func(in_frame=expC,thresh=5,trace=T)

pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, data=binCsum)
boxplot(angs~ID, data=binCsum)
boxplot (angs~time2, data=binCsum)

#lm model
BinC.lm <- lm(angs ~cond*time2, data=binCsum)

#barplots again
op=par(mfrow=c(2,2))
plot(BinC.lm)

BinC.E=rstandard(BinC.lm)
boxplot(BinC.E ~ cond, data=binCsum, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinC.E ~ time2, data=binCsum, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinC.E ~ ID, data=binCsum, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (angs ~ cond*time2)
BinC.gls<- gls(Form, data=binCsum)


#nlme model
BinC1.lme <- lme (Form, random = ~1|ID, method="REML", data=binCsum) #best

#BinC2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", data=binCsum) 

#BinC3.lme <- lme (angs ~ cond*time2, random = ~1|ID,  weights=varIdent(form=~1|ID), 
correlation=corAR1 (form=~1|ID/cond), method="REML", data=binCsum) 

#BinC4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|time2), 
correlation=corAR1 (form=~1|ID/cond), method="REML", data=binCsum) 

anova(BinC.gls, BinC1.lme)

summary(BinC1.lme)
anova(BinC1.lme)

#residuals
BinC.E2<-resid(BinC1.lme,type="normalized")
BinC.F2<-fitted(BinC1.lme)
op<-par(mfrow=c(1,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinC.F2,y=BinC.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinC.E2~cond,data=binCsum, main="cond",ylab=MyYlab)

