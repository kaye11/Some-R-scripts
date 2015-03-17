library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)

count$time=count$T*60
count$Tn=as.numeric(count$T)
count$Bin=as.numeric (count$binn)

countsum <- summarySE(count, measurevar="CellsN", groupvars=c("treatment","T", "Bin"))

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=count$Bin, treatment=count$treatment, T=count$T, A=count$A, ID=count$reptreat)))
cor(exp, method = "spearman")
cor.test(exp$bin, exp$treatment, method="spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#correlation with bin and treatment
#start with simplest formula

#basic model

SF <- gam(CellsN ~ treatment, data=count)

SF1 <- gam(CellsN ~ treatment + Tn + Bin, data=count) 
SF2 <- gam(CellsN ~ treatment + s(Tn, by=treatment) + Bin, data=count)
SF3 <- gam(CellsN ~ treatment + s(Tn, by=treatment), data=count) 
SF4 <- gam(CellsN ~ s(Tn, by=treatment), data=count) #best
SF5 <- gam(CellsN ~ s(Tn, by=treatment) + binn, data=count) 
SF51 <- gam(CellsN ~ s(Tn, by=treatment) + te(treatment, Bin, by=treatment, bs=rep("cs", 2)), data=count) 

#make splines/smoothers

SF6 <- gam(CellsN ~ s(Tn, by=treatment, bs="cr"), data=count) 

SF7 <- gam(CellsN ~ s(Tn, by=treatment, bs="cs"), data=count) #best

SF71 <- gam(CellsN ~ s(Tn, by=treatment, bs="cs") + ti(Tn, binn, by=treatment, k=3), data=count) #best

SF72 <- gam(CellsN ~ s(Tn, by=treatment, bs="cs") + ti(Tn, binn, by=treatment, k=3) +s (reptreat, bs="re"), 
            data=count) #best


#add random effects

SF8 <- gam(CellsN ~ s(Tn, by=treatment, bs="cs") + s (reptreat, bs="re"), data=count) #best

#try gamm

G1 <-  gamm(CellsN ~ s(Tn, by=treatment, bs="cs"), random = list(reptreat=~1), data=count)
summary(G1)
plot(G1$gam)
anova(G1$gam)
summary(G1$gam)

#try nesting

G2 <-  gamm(CellsN ~ s(Tn, by=treatment, bs="cs"), random = list(reptreat=~1/treatment), data=count) #same as G1

#try correlation

f1 <- CellsN ~ s(Tn, by=treatment, bs="cs")

G <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs"), method="REML", data=count)

G3 <- gamm(f1, method="REML", correlation= corCompSymm (form=~Tn), data=count)
G4 <- gamm(f1, method="REML", correlation= corCompSymm (form=~Bin), data=count)
G5 <- gamm(f1, method="REML", correlation= corCompSymm (form=~1|treatment/Bin), data=count) #AIC 384
G6 <- gamm(f1, method="REML", correlation= corCompSymm (form=~1|treatment/Tn), data=count)
G7 <- gamm(f1, method="REML", correlation= corCompSymm (form=~1|treatment/reptreat), data=count)
G8 <- gamm(f1, method="REML", correlation= corCompSymm (form=~reptreat), data=count)

AIC(G$lme, G3$lme, G4$lme, G5$lme, G6$lme, G7$lme, G8$lme)


G9 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/Bin), data=count)
G10 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/Tn), data=count)
G11 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) #AIC 520

AIC(G9$lme, G10$lme, G11$lme)

cs1 <- corARMA(c(0.2), p = 1, q = 0)
cs2 <- corARMA(c(0.3, -0.3), p = 2, q = 0)
cs3 <- corARMA(c(0.5, -0.5), p = 2, q = 0)
G12 <- gamm(f1, method="REML", correlation= cs1, data=count)
G13 <- gamm(f1, method="REML", correlation= cs2, data=count)
G14 <- gamm(f1, method="REML", correlation= cs3, data=count)


#weights

vf1 <- varIdent (form = ~T)

W1 <- gamm(f1, method="REML", weights = varExp (form=~ 1 | Tn), data=count)
W2 <- gamm(f1, method="REML", weights = varExp (form=~ Bin), data=count)
W3 <- gamm(f1, method="REML", weights = varExp (form=~ treatment|Bin), data=count) 
W4 <- gamm(f1, method="REML", weights = varExp (form=~ treatment|Tn), data=count)
W5 <- gamm(f1, method="REML", weights = varExp (form=~ treatment|reptreat), data=count)
W6 <- gamm(f1, method="REML", weights = varExp (form=~ reptreat), data=count)

AIC (W1$lme, W2$lme, W3$lme, W4$lme, W5$lme, W6$lme)

g4 <- gamm (CellsN ~ s(T, by=treatment, bs="cs"), random = ~ (binn) + (Tn), data=count)

W7 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varIdent(form=~1| reptreat), data=count) #best

W8 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varIdent(form=~1| Bin), data=count)

W9 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varIdent(form=~1| Tn), data=count)

W10 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varIdent(form=~treatment/reptreat), data=count)

W11 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~Tn/Bin), data=count)

W12 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~treatment/Tn), data=count)

W13 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~treatment/Bin), data=count)

## making VarIdent with form=~1|factor is more effective in lowering AICs than form=~factor/factor1 and the
## form=~1|factor/factor 1 is an invalid formula

W14 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varComb (varIdent(form=~1| reptreat), varExp(form= ~Tn)), data=count)

W15 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varComb (varIdent(form=~1| reptreat), varExp(form= ~binn)), data=count)

W16 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varComb (varIdent(form=~1| reptreat), varPower(form= ~binn)), data=count)

W17 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varComb (varIdent(form=~1| reptreat), varConstPower(form= ~treatment|reptreat)), data=count)


G5 <- gamm(f1, method="REML", correlation= corCompSymm (form=~1|treatment/binn), data=count) #AIC 384

W18 <- gamm(f1, method="REML", correlation= corCompSymm (form=~1|treatment/binn), 
           weights = varIdent(form=~reptreat), data=count) #same with G5 :(

W19 <- gamm(f1, method="REML", correlation= corCompSymm (form=~1|treatment/binn), 
            weights = varExp(form=~ binn), data=count)


##choose between models
#smooth terms
f1 <- CellsN ~ s(Tn, by=treatment, bs="cs")
SF7 <- gamm(f1, method="REML",  data=count) #best
G5 <- gamm(f1, method="REML", correlation= corCompSymm (form=~1|treatment/binn), data=count) #AIC 384
G11 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) #AIC 520
W7 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varIdent(form=~1| reptreat), data=count) #best

#smooth terms plus tensor interaction
f2 <- CellsN ~ s(Tn, by=treatment, bs="cs") + ti(Tn, binn, by=treatment, k=3)
SF71 <- gamm (f2, method="REML", data=count) 
#G5A <- gamm(f2, method="REML", correlation= corCompSymm (form=~1|treatment/binn), data=count) #did not run
G11A <- gamm(f2, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7A <- gamm(f2, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           weights = varIdent(form=~1| reptreat), data=count) #best

#tensor interaction only
f3 <- CellsN ~ ti(Tn, binn, by=treatment, k=3, bs="cr")
SF73 <- gamm(f3, method="REML",  data=count)
#G5B <- gamm(f3, method="REML", correlation= corCompSymm (form=~1|treatment/binn), data=count) #did not run
G11B <- gamm(f3, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7B <- gamm(f3, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~1| reptreat), data=count) #best

#tensor interaction, no nesting
f4 <- CellsN ~ ti(Tn, by=treatment, bs="cr")
SF74 <- gamm(f4, method="REML", data=count)
#G5C <- gamm(f4, method="REML", correlation= corCompSymm (form=~1|treatment/binn), data=count) #did not run
G11C <- gamm(f4, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7C <- gamm(f4, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~1| reptreat), data=count) #best


#tensor and tensor 
f5 <- CellsN ~ ti(Tn, by=treatment, bs="cs") + ti(Tn, binn, by=treatment, k=3)
SF75 <- gamm (f5, method="REML", data=count) 
#G5D <- gamm(f5, method="REML", correlation= corCompSymm (form=~1|treatment/binn), data=count) #did not run
G11D <- gamm(f5, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=count) 
W7D <- gamm(f5, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
            weights = varIdent(form=~1| reptreat), data=count) #best



AIC(SF7$lme, G5$lme, G11$lme, W7$lme, SF71$lme, G11A$lme, W7A$lme, SF73$lme, G11B$lme, W7B$lme)

