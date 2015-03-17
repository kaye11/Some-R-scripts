library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

# Combine A and cond
binned$Ac <- paste(binned$A, binned$cond, sep = "-")

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=binned$binn, cond=binned$cond, T=binned$T, A=binned$Acn)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(Vlog~cond, data=binned)
boxplot(Vlog~Acn, data=binned)
boxplot (Vlog~binn, data=binned)
boxplot (Vlog~T, data=binned)

#summaries
source("summarySE.R")
Speedsum <- summarySE(binned, measurevar="Vlog", groupvars=c("Acn"))
SpeedsumT <- summarySE(binned, measurevar="Vlog", groupvars=c("T"))
SpeedsumCond <- summarySE(binned, measurevar="Vlog", groupvars=c("cond"))



#simplest formula

SF <- gam (Vlog~cond, data=binned)
SF1 <- gam(Vlog ~ cond + T + binn, data=binned) 
SF2 <- gam(Vlog ~ cond + s(T, by=cond) + binn, data=binned) 
SF3 <- gam(Vlog ~ cond + s(T, by=cond), data=binned) 
SF4 <- gam(Vlog ~ s(T, by=cond), data=binned) 
SF5 <- gam(Vlog ~ s(T, by=cond) + binn, data=binned) 
SF6 <- gam(Vlog ~ ti(T, binn, by=cond, k=3), data=binned)
SF7 <- gam(Vlog ~ s(T, by=cond) + ti(T, binn, by=cond, k=3), data=binned) 
SF8 <- gam(Vlog ~ ti(T, by=cond) + ti(T, binn, by=cond, k=3), data=binned) 
SF9 <- gam(Vlog ~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cr"), data=binned) 
SF10 <- gam(Vlog ~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cr") + s (Acn, bs="re"), data=binned) 
SF11 <- gam(Vlog ~ s(T, by=cond, bs="cs") + ti(T, binn, by=cond, k=3, bs="cr"), data=binned) 
SF12 <- gam(Vlog ~ s(T, by=cond, bs="cs") + ti(T, binn, by=cond, k=3, bs="cr") + s (Acn, bs="re"), data=binned) 
SF13 <- gam(Vlog ~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cs"), data=binned) 
SF14 <- gam(Vlog ~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cs") + s (Acn, bs="re"), data=binned) 

SF10 <- gam(Vlog ~ s(T, by=cond, bs="cr", k=9) + ti(T, binn, by=cond, k=3, bs="cr") + s (Acn, bs="re"), data=binned) 
SF12 <- gam(Vlog ~ s(T, by=cond, bs="cs", k=9) + ti(T, binn, by=cond, k=3, bs="cr") + s (Acn, bs="re"), data=binned) 
#SF10 is the best



AIC(SF1, SF2, SF3, SF4, SF5, SF6, SF7, SF8, SF9, SF10, SF11, SF12, SF13, SF14)

#insert correlation terms

f1 <- (Vlog ~ s(T, by=cond, bs="cr", k=9) + s(binn, by=cond, bs="cr", k=3) + ti(T, binn, by=cond, k=3, bs="cr"))

G <- gamm (f1, method="REML", data=binned)
G1 <- gamm (f1, method="REML", correlation = corCompSymm (form = ~ 1|cond/Acn), data=binned)
G2 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/Acn), data=binned) 
G3 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/Acn), random=list(Acn=~1), data=binned) 
G4 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), data=binned) 
G5 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), random=list(Acn=~1), data=binned) #best


anova(G$lme, G1$lme, G2$lme, G3$lme)

#insert weights

#W1 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varIdent (form= ~1|Acn), data=binned)
W2 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varIdent (form= ~1|condn), data=binned)
W3 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varIdent (form= ~1|binn), data=binned)
W2A <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn),
             weights= varIdent (form= ~1|condn), random=list(Acn=~1), data=binned) #best
#W3A <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), 
             weights= varIdent (form= ~1|binn),  random=list(Acn=~1), data=binned)
#W4 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varIdent (form= ~1|T), data=binned)


W5 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~Acn), data=binned)
W6 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~condn), data=binned)
W7 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~binn), data=binned)
#W8 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~T), data=binned)


#W9 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~binn|Acn), data=binned)
#W10 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~condn|Acn), data=binned)
#W11 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~T|An), data=binned)
#W12 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varPower (form= ~binn|condn), data=binned)

#W13 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/Acn), weights= varConstPower (form= ~binn|Acn), data=binned)
#W14 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/Acn), weights= varConstPower (form= ~condn|Acn), data=binned)
#W15 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/Acn), weights= varConstPower (form= ~T|Acn), data=binned)
#W16 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/Acn), weights= varConstPower (form= ~binn|condn), data=binned)

#W17 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varConstPower (form= ~Acn), data=binned)
#W18 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varConstPower (form= ~condn), data=binned)
#W19 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varConstPower (form= ~binn), data=binned)
#W20 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|binn/Acn), weights= varConstPower (form= ~T), data=binned)


W21 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/Acn), random=list(Acn=~1),
             weights= varComb (varIdent (form= ~1|condn), varPower (form= ~binn|condn)), data=binned)

#**W1 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), weights= varIdent (form= ~1|An), data=binned)

#*W12A <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), weights= varPower (form= ~condn|binn), data=binned)
#*W12B <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), weights= varPower (form= ~condn|T), data=binned)
#*W12C <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), weights= varPower (form= ~binn|T), data=binned)
#*W12D <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), weights= varPower (form= ~T|binn), data=binned)

#*not meaningful for factors
#**iteration reached without convergence


anova (G2$lme, W2$lme, W3$lme, W4$lme, W5$lme, W6$lme, W7$lme, W12$lme, W17$lme, W19$lme, W21$lme)