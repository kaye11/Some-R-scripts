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

#Bin A
BinA= subset (count, Bin=='A')
expA=as.data.frame(data.table(cbind(treatment=BinA$treatment, T=BinA$T, ID=BinA$reptreat)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsN~treatment, data=BinA)
boxplot(CellsN~reptreat, data=BinA)
boxplot (CellsN~T, data=BinA)

#lm model
BinA.lm <- lm(CellsN ~treatment*T, data=BinA)

#barplots again
op=par(mfrow=c(2,2))
plot(BinA.lm)

BinA.E=rstandard(BinA.lm)
boxplot(BinA.E ~ treatment, data=BinA, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinA.E ~ T, data=BinA, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinA.E ~ reptreat, data=BinA, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (CellsN ~ treatment*T)
BinA.gls<- gls(Form, data=BinA)


#nlme model
BinA1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinA)

BinA2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinA)

BinA3.lme <- lme (CellsN ~ treatment*T, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinA) #best

BinA4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), 
                  correlation=corAR1 (), method="REML", data=BinA) 

anova(BinA.gls, BinA1.lme, BinA2.lme, BinA3.lme, BinA4.lme)

summary(BinA3.lme)
anova(BinA3.lme)

#residuals
BinA.E2<-resid(BinA3.lme,type="normalized")
BinA.F2<-fitted(BinA3.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinA.F2,y=BinA.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinA.E2~treatment,data=BinA, main="Treatment",ylab=MyYlab)
plot(x=BinA$T,y=BinA.E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinA.E2 ~ T| treatment, data=BinA, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

#try gamm
BinA.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs"),  random = list(reptreat=~1),  
                 weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), 
                 method="REML", data=count) 

summary(BinA.gamm$gam) #Si significant
anova(BinA.gamm$gam) # Si significant
plot(BinA.gamm$gam) #things look linear, more support for using nlme
plot(BinA.gamm$lme) # patterns seen on the residuals
summary(BinA.gamm$lme)


###BIN B

#BinB
BinB= subset (count, Bin=='B')
expB=as.data.frame(data.table(cbind(treatment=BinB$treatment, T=BinB$T, A=BinB$A, ID=BinB$reptreat)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)
pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsN~treatment, data=BinB)
boxplot(CellsN~reptreat, data=BinB)
boxplot (CellsN~T, data=BinB)

#lm model
BinB.lm <- lm(CellsN ~treatment*T, data=BinB)

#barplots again
op=par(mfrow=c(2,2))
plot(BinB.lm)

BinB.E=rstandard(BinB.lm)
boxplot(BinB.E ~ treatment, data=BinB, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinB.E ~ T, data=BinB, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinB.E ~ reptreat, data=BinB, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (CellsN ~ treatment*T)
BinB.gls<- gls(Form, data=BinB)


#nlme model
BinB1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinB)

BinB2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinB)

BinB3.lme <- lme (CellsN ~ treatment*T, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinB) #best

BinB4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), 
                  correlation=corAR1 (), method="REML", data=BinB) 

anova(BinB.gls, BinB1.lme, BinB2.lme, BinB3.lme, BinB4.lme)

summary(BinB3.lme)
anova(BinB3.lme)

#residuals
BinB.E2<-resid(BinB3.lme,type="normalized")
BinB.F2<-fitted(BinB3.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinB.F2,y=BinB.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinB.E2~treatment,data=BinB, main="Treatment",ylab=MyYlab)
plot(x=BinB$T,y=BinB.E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinB.E2 ~ T| treatment, data=BinB, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

#try gamm
BinB.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs"),  random = list(reptreat=~1),  
                   weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), 
                   method="REML", data=count) 

summary(BinB.gamm$gam) #Si significant
anova(BinB.gamm$gam) # Si significant
plot(BinB.gamm$gam) #things look linear, more support for using nlme
plot(BinB.gamm$lme) # patterns seen on the residuals
summary(BinB.gamm$lme)

##BIN C

#BinC

BinC= subset (count, Bin=='C')
expC=as.data.frame(data.table(cbind(treatment=BinC$treatment, T=BinC$T, A=BinC$A, ID=BinC$reptreat)))
cor(expC, method = "spearman")

vif_func(in_frame=expC,thresh=5,trace=T)
pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsN~treatment, data=BinC)
boxplot(CellsN~reptreat, data=BinC)
boxplot (CellsN~T, data=BinC)

#lm model
BinC.lm <- lm(CellsN ~treatment*T, data=BinC)

#barplots again
op=par(mfrow=c(2,2))
plot(BinC.lm)

BinC.E=rstandard(BinC.lm)
boxplot(BinC.E ~ treatment, data=BinC, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinC.E ~ T, data=BinC, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinC.E ~ reptreat, data=BinC, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (CellsN ~ treatment*T)
BinC.gls<- gls(Form, data=BinC)


#nlme model
BinC1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinC)

BinC2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinC)

BinC3.lme <- lme (CellsN ~ treatment*T, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinC) #best

BinC4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), 
                  correlation=corAR1 (), method="REML", data=BinC) 

anova(BinC.gls, BinC1.lme, BinC2.lme, BinC3.lme, BinC4.lme)

summary(BinC3.lme)
anova(BinC3.lme)

#residuals
BinC.E2<-resid(BinC3.lme,type="normalized")
BinC.F2<-fitted(BinC3.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinC.F2,y=BinC.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinC.E2~treatment,data=BinC, main="Treatment",ylab=MyYlab)
plot(x=BinC$T,y=BinC.E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinC.E2 ~ T| treatment, data=BinC, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

#try gamm
BinC.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs"),  random = list(reptreat=~1),  
                   weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), 
                   method="REML", data=count) 

summary(BinC.gamm$gam) #Si significant
anova(BinC.gamm$gam) # Si significant
plot(BinC.gamm$gam) #things look linear, more support for using nlme
plot(BinC.gamm$lme) # patterns seen on the residuals
summary(BinC.gamm$lme)



