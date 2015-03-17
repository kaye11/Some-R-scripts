library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

write.table (sub, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/substrate.csv", 
             sep=";", col.names=T, row.names=F)

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=sub$Bin, treatment=sub$treatment, T=sub$T, A=sub$A, ID=sub$reptreat)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

source("summarySE.R")
dfc2 <- summarySE(sub, measurevar="CellsN", groupvars=c("reptreat"))

#boxplots

boxplot(CellsN~treatment, data=sub)
boxplot(CellsN~reptreat, data=sub)
boxplot (CellsN~Bin, data=sub)
boxplot (CellsN~T, data=sub)


#full model simple

SF <- gam (CellsN ~ s(T, by=treatment), method="REML", data=sub)
SF1 <- gam (CellsN ~ s(T, by=treatment) + s (binn, by=treatment, k=3), method="REML", data=sub)
SF2 <- gam (CellsN ~ s(T, by=treatment) + s (binn, by=treatment, k=3) + ti (T, binn, by=treatment, k=3), 
            method="REML", data=sub)
SF2A <- gam (CellsN ~ s(T, by=treatment, bs="cs", k=3) + s (binn, by=treatment, k=3) + 
               ti (T, binn, by=treatment, bs="cs", k=3), method="REML", data=sub)
SF3 <- gam (CellsN ~ s(T, by=treatment, bs="cs", k=3) + s (binn, by=treatment, k=3) + 
              ti (T, binn, by=treatment, bs="cs", k=3) + s(reptreat, bs="re"), method="REML", data=sub)

AIC(SF, SF1, SF2, SF2A, SF3)

#GAMMs

f1 <- CellsN ~ s(T, by=treatment, bs="cs", k=3) + s (binn, by=treatment, k=3) + ti (T, binn, by=treatment, bs="cs", k=3)

G <- gamm (f1, method="REML", data=sub)
G1 <- gamm (f1, random=list(reptreat=~1), method="REML", data=sub)
G2 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data=sub) # best model
G3 <- gamm(f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), data=sub)
G4 <- gamm(f1, method="REML", weights=varIdent(form=~1|reptreat), data=sub) # iteration without convergence

plot(G2$gam,pages=4,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
anova(G$lme, G1$lme, G2$lme, G3$lme)
