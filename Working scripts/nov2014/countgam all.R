library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)

count$T=count$T/60

countsum <- summarySE(count, measurevar="CellsN", groupvars=c("treatment","T", "Bin"))

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=count$bin, treatment=count$treatment, T=count$T, A=count$A)))
cor(exp, method = "spearman")
cor.test(exp$bin, exp$treatment, method="spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#correlation with bin and treatment
#start with simplest formula

SF <- gam(CellsN ~ treatment + T + Bin, data=count)

SF1 <- gam(CellsN ~ treatment + s(T) + Bin, data=count)
SF2 <- gam(CellsN ~ treatment + s(T), data=count)

SF3 <- gam(CellsN ~  s(T,by = as.numeric(treatment == "Control", bs="cr")) + 
             s(T,by = as.numeric(treatment == "Si", bs="cr")),data = count)
SF4 <- gam(CellsN ~  s(T,by = as.numeric(treatment == "Control", bs="cr")) + 
             s(T,by = as.numeric(treatment == "Si", bs="cr")) + s(reptreat, bs="re"),data = count)

#combined bin and treatment
SF5 <- gam(CellsN ~  s(T,by = as.numeric(bintreat == "A-Con", bs="cr")) +
             s(T,by = as.numeric(bintreat == "B-Con", bs="cr")) +
             s(T,by = as.numeric(bintreat == "C-Con", bs="cr")) +
             s(T,by = as.numeric(bintreat == "A-Si", bs="cr")) +
             s(T,by = as.numeric(bintreat == "B-Si", bs="cr")) +
             s(T,by = as.numeric(bintreat == "C-Si", bs="cr")), data = count)

SF6 <- gam(CellsN ~  s(T,by = as.numeric(bintreat == "A-Con", bs="cr")) +
             s(T,by = as.numeric(bintreat == "B-Con", bs="cr")) +
             s(T,by = as.numeric(bintreat == "C-Con", bs="cr")) +
             s(T,by = as.numeric(bintreat == "A-Si", bs="cr")) +
             s(T,by = as.numeric(bintreat == "B-Si", bs="cr")) +
             s(T,by = as.numeric(bintreat == "C-Si", bs="cr"))+ s (reptreat, bs="re"), data = count)

SF7 <-gam(CellsN ~  s(T,by = as.numeric(reptreat == "1-Con", bs="cr")) + 
  s(T,by = as.numeric(reptreat == "2-Con", bs="cr")) +
  s(T,by = as.numeric(reptreat == "3-Con", bs="cr")) +
  s(T,by = as.numeric(reptreat == "1-Si", bs="cr")) +
  s(T,by = as.numeric(reptreat == "2-Si", bs="cr")) +
  s(T,by = as.numeric(reptreat== "3-Si", bs="cr")) + s (reptreat, bs="re"), data = count)


#correlation structures

f1 <- CellsN ~  s(T,by = as.numeric(bintreat == "A-Con", bs="cr")) + 
  s(T,by = as.numeric(bintreat == "B-Con", bs="cr")) +
  s(T,by = as.numeric(bintreat == "C-Con", bs="cr")) +
  s(T,by = as.numeric(bintreat == "A-Si", bs="cr")) +
  s(T,by = as.numeric(bintreat == "B-Si", bs="cr")) +
  s(T,by = as.numeric(bintreat == "C-Si", bs="cr"))+ s (reptreat, bs="re")

C1 <- gamm(CellsN ~ treatment + s(T), data = count, correlation = corCompSymm (form = ~ Bin|treatment))
C2 <- gamm(CellsN ~ bintreat + s(T), data = count, correlation = corCompSymm (form = ~ Bin|treatment))

BM2<-gamm(Birds~Rain+ID+
            s(Time,by=ID),
          correlation=corAR1(form=~Time |ID ),
          weights=varIdent(form=~1|ID))

#using bintreat as factor, tunay na ito

BT <- gam (CellsN ~ bintreat + s (T, by=bintreat), data=count)
BT1 <- gam (CellsN ~ bintreat + s (T, by=bintreat) + s(reptreat, bs="re"), data=count)
BT2 <- gam (CellsN ~ bintreat + s (T, by=bintreat, bs="cr") + s(reptreat, bs="re"), data=count) #best
BT3 <- gam (CellsN ~ bintreat + s (T, by=bintreat, bs="cs") + s(reptreat, bs="re"), data=count)


#correlation structures
f1 <- CellsN ~ bintreat + s (T, by=bintreat, bs="cr") + s(reptreat, bs="re")

CBT <- gamm(f1, correlation=corCompSymm(form = ~T|reptreat), weights = varIdent (form= ~1 | reptreat), data=count)

##combining bin and treatment would not be right so just treat them separately and put it correlation and weights

