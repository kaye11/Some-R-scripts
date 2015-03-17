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

#Bin A
BinA= subset (count, Bin=='A')
expA=as.data.frame(data.table(cbind(treatment=BinA$treatment, T=BinA$T, A=BinA$A, ID=BinA$reptreat)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)
corvif(expA)
pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(CellsN~treatment, data=BinA)
boxplot(CellsN~reptreat, data=BinA)
boxplot (CellsN~Tn, data=BinA)

BA <- gamm (CellsN~s(Tn, by=treatment), method="REML", data = BinA)
BA1 <- gamm (CellsN~s(Tn, by=treatment, bs="cr", k=4), method="REML", data = BinA)
BA2 <- gamm (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", data = BinA) #best
BA3 <- gamm (CellsN~te(Tn, by=treatment), method="REML", data = BinA) 


fBin <- CellsN~s(Tn, by=treatment, bs="cs", k=4)

BA4 <- gamm (fBin, method="REML",  random=list(reptreat=~1), data = BinA) #same with BA2
BA5 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data = BinA) 
BA6 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), weights = varIdent(form=~1| reptreat), 
             data = BinA) #best
BA7 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), 
             data = BinA)

anova(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme, BA6$lme, BA7$lme)

A <- gam (CellsN~s(Tn, by=treatment), method="REML", data = BinA)
A1 <- gam (CellsN~s(Tn, by=treatment, bs="cr", k=4), method="REML", data = BinA)
A2 <- gam (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", data = BinA) 
A3 <- gam (CellsN~te(Tn, by=treatment), method="REML", data = BinA) 
A4 <- gam (CellsN~s(Tn, by=treatment, bs="cs", k=4) + s(reptreat, bs="re"), method="REML", data = BinA) 

AIC (A, A1, A2, A3, A4)

#BinB
BinB= subset (count, Bin=='B')
expB=as.data.frame(data.table(cbind(treatment=BinB$treatment, T=BinB$T, A=BinB$A, ID=BinB$reptreat)))
cor(expB, method = "spearman")


vif_func(in_frame=expB,thresh=5,trace=T)
corvif(expB)
pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(CellsN~treatment, data=BinB)
boxplot(CellsN~reptreat, data=BinB)
boxplot (CellsN~Tn, data=BinB)

BB <- gamm (CellsN~s(Tn, by=treatment), method="REML", data = BinB)
BB1 <- gamm (CellsN~s(Tn, by=treatment, bs="cr", k=4), method="REML", data = BinB)
BB2 <- gamm (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", data = BinB) #best
BB3 <- gamm (CellsN~te(Tn, by=treatment), method="REML", data = BinB) 


fBin <- CellsN~s(Tn, by=treatment, bs="cs", k=4)

BB4 <- gamm (fBin, method="REML",  random=list(reptreat=~1), data = BinB) #same with BB2
BB5 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data = BinB) 
BB6 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), weights = varIdent(form=~1| reptreat), 
             data = BinB) #best
BB7 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), 
             data = BinB)

anova(BB$lme, BB1$lme, BB2$lme, BB3$lme, BB4$lme, BB5$lme, BB6$lme, BB7$lme)

B <- gam (CellsN~s(Tn, by=treatment), method="REML", data = BinB)
B1 <- gam (CellsN~s(Tn, by=treatment, bs="cr", k=4), method="REML", data = BinB)
B2 <- gam (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", data = BinB) 
B3 <- gam (CellsN~te(Tn, by=treatment), method="REML", data = BinB) 
B4 <- gam (CellsN~s(Tn, by=treatment, bs="cs", k=) + s(reptreat, bs="re"), method="REML", data = BinB) 

AIC (B, B1, B2, B3, B4)

#BinC

BinC= subset (count, Bin=='C')
expC=as.data.frame(data.table(cbind(treatment=BinC$treatment, T=BinC$T, A=BinC$A, ID=BinC$reptreat)))
cor(expC, method = "spearman")


vif_func(in_frame=expC,thresh=5,trace=T)
corvif(expC)
pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(CellsN~treatment, data=BinC)
boxplot(CellsN~reptreat, data=BinC)
boxplot (CellsN~Tn, data=BinC)

BC <- gamm (CellsN~s(Tn, by=treatment), method="REML", data = BinC)
BC1 <- gamm (CellsN~s(Tn, by=treatment, bs="cr", k=4), method="REML", data = BinC)
BC2 <- gamm (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", data = BinC) #best
BC3 <- gamm (CellsN~te(Tn, by=treatment), method="REML", data = BinC) 


fBin <- CellsN~s(Tn, by=treatment, bs="cs", k=4)

BC4 <- gamm (fBin, method="REML",  random=list(reptreat=~1), data = BinC) #same with BC2
BC5 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data = BinC) 
BC6 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), weights = varIdent(form=~1| reptreat), 
             data = BinC) #best
BC7 <- gamm (fBin, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), 
             data = BinC)

anova(BC$lme, BC1$lme, BC2$lme, BC3$lme, BC4$lme, BC5$lme, BC6$lme, BC7$lme)

C <- gam (CellsN~s(Tn, by=treatment), method="REML", data = BinC)
C1 <- gam (CellsN~s(Tn, by=treatment, bs="cr", k=4), method="REML", data = BinC)
C2 <- gam (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", data = BinC) 
C3 <- gam (CellsN~te(Tn, by=treatment), method="REML", data = BinC) 
C4 <- gam (CellsN~s(Tn, by=treatment, bs="cs", k=4) + s(reptreat, bs="re"), method="REML", data = BinC) 

AIC (C, C1, C2, C3, C4)
