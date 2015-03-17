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
exp=as.data.frame(data.table(cbind(bin=binned$bin, cond=binned$cond, time=binned$time, ID=binned$ID)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T) 
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,3))
boxplot(Vlog~cond, data=binned)
boxplot(Vlog~ID, data=binned)
boxplot (Vlog~bin, data=binned)
boxplot (Vlog~time, data=binned)
boxplot (Vlog~T, data=binned)

#summaries
source("summarySE.R")
Speedsum <- summarySE(binned, measurevar="Vlog", groupvars=c("ID"))
SpeedsumT <- summarySE(binned, measurevar="Vlog", groupvars=c("time"))
SpeedsumCond <- summarySE(binned, measurevar="Vlog", groupvars=c("cond"))

#bestmodel WD
binned$binn=as.numeric(binned$bin)
binned$condn=as.numeric(binned$condn)

WDvel <- gamm (Vlog ~ s(T, by=cond, bs="cr", k=9) + s(binn, by=cond, bs="cr", k=3) + 
                 ti(T, binn, by=cond, k=3, bs="cr"), method="REML", 
               correlation = corAR1 (form = ~ 1|binn/ID), weights= varIdent (form= ~1|condn), 
               random=list(ID=~1), data=binned) #best


