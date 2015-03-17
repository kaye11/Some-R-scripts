library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=count$Bin, treatment=count$treatment, T=count$T, A=count$A, ID=count$reptreat)))
cor(exp, method = "spearman")
cor.test(exp$bin, exp$treatment, method="spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(CellsN~treatment, data=count)
boxplot(CellsN~reptreat, data=count)
boxplot (CellsN~binn, data=count)
boxplot (CellsN~Tn, data=count)

##choose between models
#smooth terms
f1 <- CellsN ~ s(Tn, by=treatment, bs="cs", k=4)
SF7 <- gamm(f1, method="REML", data=count) #best
G11 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) #AIC 520
W7 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varIdent(form=~1| reptreat), data=count) #best

#smooth terms plus tensor interaction
f2 <- CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + ti(Tn, binn, by=treatment, k=3)
SF71 <- gamm (f2, method="REML", data=count) 
G11A <- gamm(f2, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7A <- gamm(f2, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~1| reptreat), data=count) #best

#tensor interaction only
f3 <- CellsN ~ ti(Tn, binn, by=treatment, k=3, bs="cr")
SF73 <- gamm(f3,  method="REML", data=count)
G11B <- gamm(f3, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7B <- gamm(f3, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~1| reptreat), data=count) #best

#tensor interaction, no nesting
f4 <- CellsN ~ ti(Tn, by=treatment, bs="cr")
SF74 <- gamm(f4,  method="REML", data=count)
G11C <- gamm(f4, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7C <- gamm(f4, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~1| reptreat), data=count) #best


#tensor and tensor 
f5 <- CellsN ~ ti(Tn, by=treatment, bs="cs") + ti(Tn, binn, by=treatment, k=3)
SF75 <- gamm (f5, method="REML", data=count) 
G11D <- gamm(f5, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7D <- gamm(f5, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~1| reptreat), data=count) #best

AIC(W7$lme, W7A$lme, W7B$lme, W7C$lme, W7D$lme)
anova(W7$lme, W7A$lme, W7B$lme, W7C$lme, W7D$lme, SF71$lme, SF71A$lme, W7AA$lme, W7AB$lme, W7CB$lme, W7DB$lme)

gam.check(W7$gam)
gam.check(W7A$gam)
gam.check(W7B$gam)
gam.check(W7C$gam)
gam.check(W7D$gam)

plot(W7A$gam,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

W7AA <- gamm(f2, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            random=list(reptreat=~1, binn=~1), data=count) #best

SF71A <- gamm (f2, method="REML", random=list(reptreat=~1), data=count) 

W7AB <- gamm(f2, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
             random=list(reptreat=~1, binn=~1), weights=varIdent(form=~1|reptreat), data=count) 

W7BB <- gamm(f3, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
             random=list(reptreat=~1, binn=~1), weights = varIdent(form=~1| reptreat), data=count) #iteration problem

W7CB <- gamm(f4, method="REML", correlation= corAR1 (form=~1|treatment/reptreat),              
             random=list(reptreat=~1, binn=~1), weights = varIdent(form=~1| reptreat), data=count) 

W7DB <- gamm(f5, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
             random=list(reptreat=~1, binn=~1), weights = varIdent(form=~1| reptreat), data=count) 

anova(W7$lme, W7A$lme, W7B$lme, W7C$lme, W7D$lme, SF71$lme, SF71A$lme, W7AA$lme, W7AB$lme, W7CB$lme, W7DB$lme, W7AB1$lme)

plot(W7AB$gam,pages=1)

W7AB1 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=3) + ti(Tn, binn, by=treatment, k=3), 
             method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
             random=list(reptreat=~1, binn=~1), weights=varIdent(form=~1|reptreat), data=count) #best choice

#residuals with W7 and W7CB have patterns

W7AB1 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=3) + ti(Tn, binn, by=treatment, k=3), 
              method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
              random=list(reptreat=~1), weights=varIdent(form=~1|reptreat), data=count) #iteration problem

W7AB2 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=3) + ti(Tn, binn, by=treatment, k=3), 
              method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
              random=list(reptreat=~1), data=count) 

W7B1 <- gamm(f3, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
             random=list(reptreat=~1), weights = varIdent(form=~1| reptreat), data=count) #iteration problem

W7B2 <- gamm(f3, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
             weights = varIdent(form=~1| reptreat), data=count) #iteration problem


W8 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + s(binn, by=treatment, bs="cs", k=3) + 
             ti(Tn, binn, by=treatment, k=3), method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights=varIdent(form=~1|reptreat), data=count) #use this as model
AIC(W8$lme)
plot(W8$gam,pages=2)
summary(W8)

#smoother for time by treatment, smoother for bin by treatment, tensor interaction produces smoothing for 
#main effecs and lower interactions



W9 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + s(binn, by=treatment, bs="cs", k=3) + te(Tn, binn, by=treatment, k=3), 
           method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           random=list(reptreat=~1), data=count) #te as smoother

w10 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + s(binn, by=treatment, bs="cs", k=3),  
            method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            random=list(reptreat=~1), data=count) #no interaction

W11 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + s(binn, by=treatment, bs="cs", k=3) + 
             ti(Tn, binn, by=treatment, k=3), method="REML", data=count) #no correlation and weights

anova(W7$lme, W7A$lme, W7B$lme, W7C$lme, W7D$lme, W8$lme, W9$lme, W10$lme, W11$lme)

W12 <- gamm(CellsN ~ te(Tn, by=treatment) + te(binn, by=treatment, bs="cr", k=3) + 
             ti(Tn, binn, by=treatment, k=3), method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights=varIdent(form=~1|reptreat), data=count)#tensor smoothing

countsum2 <- summarySE(count, measurevar="CellsN", groupvars="reptreat")

W13 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + ti(Tn, binn, by=treatment, k=3), method="REML", 
            correlation= corAR1 (form=~1|treatment/reptreat), 
           weights=varIdent(form=~1|reptreat), data=count) #no smoothing by bin

anova(W7$lme, W7A$lme, W7B$lme, W7C$lme, W7D$lme, W8$lme, W9$lme, W10$lme, W11$lme, W12$lme, )

plot(resid(W8$lme, type="normalized"))

resW8 <- resid(W8$lme, type="normalized")
fitW8<- fitted.values(W8$lme)
obsW8 <- sqrt(count$CellsN)
plot(fitW8, obsW8)
plot(W8$gam,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

resW13 <- resid(W13$lme, type="normalized")
fitW13<- fitted.values(W13$lme)
obsW13 <- sqrt(count$CellsN)
plot(fitW13, obsW13)
plot(W13$gam,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

resW13 <- resid(W13$lme, type="normalized")
fitW13<- fitted.values(W13$lme)
obsW13 <- sqrt(count$CellsN)
plot(fitW13, obsW13)
plot(W13$gam,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

