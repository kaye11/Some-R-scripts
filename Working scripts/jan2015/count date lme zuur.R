library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(nlme)


#The good approach in Zuur et al. 2009

#summaries
source("summarySE.R")
countsum <- summarySE(count, measurevar="CellsN", groupvars=c("reptreat"))

#check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=count$Bin, treatment=count$treatment, T=count$T, A=count$A, ID=count$reptreat)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)

pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))

boxplot(CellsN~treatment, data=count)
boxplot(CellsN~reptreat, data=count)
boxplot (CellsN~Bin, data=count)
boxplot (CellsN~T, data=count)

#normality test
by(count$CellsN, count$reptreat, shapiro.test) 

shapiro.test(count$CellsN)

#homogeneity of variance
#  the null hypothesis is that all populations variances are equal; 
# the alternative hypothesis is that at least two of them differ.
qplot(reptreat, CellsN, data =count,  geom = "boxplot")
bartlett.test(CellsN ~ reptreat, data=count) #for normal data set
library(lawstat)
levene.test(count$CellsN, group=count$reptreat, location="mean")
levene.test(count$CellsN, group=count$T, location="mean")
levene.test(count$CellsN, group=count$treatment, location="mean")
levene.test(count$CellsN, group=count$Bin, location="mean")


#Step 1 Start with many explanatory variables as possible, use lm

M.lm <- lm(CellsN ~ treatment + Bin + T + treatment*Bin + treatment*T + T*Bin + treatment*T*Bin, data=count)

op=par(mfrow=c(2,2))
plot(M.lm)

E=rstandard(M.lm)
boxplot(E ~ Bin, data=count, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ treatment, data=count, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ T, data=count, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ reptreat, data=count, axes=TRUE)
abline(0,0); axis=(2)

#Step 2. Fit with gls
Form <- formula (CellsN ~ treatment + Bin + T + treatment*Bin + treatment*T + T*Bin + treatment*T*Bin)
M.gls<- gls(Form, data=count)

#Step 3 and 4 Choose a Variance structure, deciding on random factor is also part of this step and fit it

M1.lme <- lme(Form, random = ~1|reptreat, method="REML", data=count)
M2.lme <- lme(Form, random = ~1|reptreat/Bin, method="REML", data=count)
M3.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=count)
M4.lme <- lme(Form, random = ~1|reptreat/Bin, weights=varIdent(form=~1|reptreat), method="REML", data=count)
M5.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), correlation= corAR1(),
              method="REML", data=count)
M6.lme <- lme(Form, random = ~1|reptreat/Bin,  weights=varIdent(form=~1|reptreat), correlation= corAR1(),
              method="REML", data=count)
M7.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
              correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=count) # stick to this
#M8.lme <- lme(Form, random = ~1|reptreat/Bi,  weights=varIdent(form=~1|reptreat), 
              #correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=count) incompatible

# Step 5 Compare models
anova(M.gls, M1.lme, M2.lme, M3.lme, M4.lme, M5.lme, M6.lme, M7.lme)

# Step 6 Check if everything is okay
E2<-resid(M7.lme,type="normalized")
F2<-fitted(M7.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~treatment,data=count,main="Treatment",ylab=MyYlab)
boxplot(E2~Bin,data=count,main="Bin",ylab=MyYlab)
plot(x=count$T,y=E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

#Steps 7 and 8 optimal fixed structure
M1.Full <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                correlation=corAR1 (form=~1|reptreat/treatment), method="ML", data=count)

M1.A <- update (M1.Full, .~. -treatment)
M1.B <- update (M1.Full, .~. -Bin)
M1.C <- update (M1.Full, .~. -T)
anova(M1.Full, M1.A, M1.B, M1.C) # can be dropped

Form2 <- formula (CellsN ~ treatment*Bin + treatment*T + T*Bin + treatment*T*Bin)
M2.Full <- lme (Form2, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                correlation=corAR1 (form=~1|reptreat/treatment), method="ML", data=count)
M2.A <- update (M1.Full, .~. -treatment:Bin)
M2.B <- update (M1.Full, .~. -Bin:T)
anova(M2.Full, M2.A, M2.B) #can be dropped

Form3 <- formula (CellsN ~ treatment*T + treatment*T*Bin)
M3.Full <- lme (Form3, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                correlation=corAR1 (form=~1|reptreat/treatment), method="ML", data=count)
M3.A <- update (M1.Full, .~. -treatment:T) #can be dropped
M3.B <- update (M1.Full, .~. -treatment:T:Bin)
anova(M3.Full, M3.A, M3.B)

M4.final <- lme (CellsN~treatment*T*Bin, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                 correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=count ) #best

M4.A <- lme (CellsN~treatment*T*Bin, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
             correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=count )

M4.B <- lme (CellsN~treatment*T*Bin, random = ~1|reptreat/Bin,  weights=varIdent(form=~1|treatment), 
             correlation=corAR1 (), method="REML", data=count)

library(lattice)
xyplot (E2 ~ T| Bin*treatment, data=count, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

library(mgcv)
M5.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs") + s(Bin, by=treatment, bs="fs") + 
                   ti(T, Bin, by=treatment, bs="fs", k=3), random = list(reptreat=~1),  
                 weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), 
                 method="REML", data=count) 

summary(M5.gamm$gam) #no sig differences found
anova(M5.gamm$gam) # no sig differences found
plot(M5.gamm$gam) #things look linear, more support for using nlme
plot(M5.gamm$lme) # patterns seen on the residuals
summary(M5.gamm$lme)

#final model is M4.final
summary(M4.final)
anova(M4.final)

##BINS


##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")

#Bin A
BinA= subset (count, Bin=='A')
expA=as.data.frame(data.table(cbind(treatment=BinA$treatment, T=BinA$T, ID=BinA$reptreat)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

levene.test(BinA$CellsN, group=BinA$reptreat, location="mean")
levene.test(BinA$CellsN, group=BinA$T, location="mean")
levene.test(BinA$CellsN, group=BinA$treatment, location="mean")


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

BinA5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinA) 

anova(BinA.gls, BinA1.lme, BinA2.lme, BinA3.lme, BinA4.lme, BinA5.lme)

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
                   method="REML", data=BinA) 

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


levene.test(BinB$CellsN, group=BinB$reptreat, location="mean")
levene.test(BinB$CellsN, group=BinB$T, location="mean")
levene.test(BinB$CellsN, group=BinB$treatment, location="mean")


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

BinB5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinB) 

anova(BinB.gls, BinB1.lme, BinB2.lme, BinB3.lme, BinB4.lme, BinB5.lme)

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
                   method="REML", data=BinB) 

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

levene.test(BinC$CellsN, group=BinC$reptreat, location="mean")
levene.test(BinC$CellsN, group=BinC$T, location="mean")
levene.test(BinC$CellsN, group=BinC$treatment, location="mean")


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

BinC5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinC) 

anova(BinC.gls, BinC1.lme, BinC2.lme, BinC3.lme, BinC4.lme, BinC5.lme)

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
                   method="REML", data=BinC) 

summary(BinC.gamm$gam) #Si significant
anova(BinC.gamm$gam) # Si significant
plot(BinC.gamm$gam) #things look linear, more support for using nlme
plot(BinC.gamm$lme) # patterns seen on the residuals
summary(BinC.gamm$lme)

summary(glht(M4.final, covariate_average=TRUE, interaction_average=TRUE))

summary(glht(BinA3.lme, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BinB3.lme, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BinC3.lme, covariate_average=TRUE, interaction_average=TRUE))
