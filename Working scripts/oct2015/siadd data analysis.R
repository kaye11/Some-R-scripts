
#read data
siadd2 <- read.csv("D:/Karen's/PhD/R program/Processed_data/si_addition/siadd2.csv", sep=";")

library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

source("AED.R")
source("vif.R")
source("summarySE.R")

ggplot(data=siadd2, aes(y=Vlog, x=add))+geom_boxplot()+facet_grid(~cond)

sum.siadd2 <- summarySE(data = siadd2, measurevar = "V", groupvars = c("condadd"))

exp=as.data.frame(data.table(cbind(condadd=as.numeric(as.factor(siadd2$condadd)), ID=as.numeric(as.factor(siadd2$ID)))))

cor(exp, method = "spearman")
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

vif_func(in_frame=exp,thresh=5,trace=T)

#boxplots
op=par(mfrow=c(2,2))
boxplot(Vlog~condadd, data=siadd2)
boxplot(Vlog~ID, data=siadd2)
boxplot (Vlog~cond, data=siadd2)

library(lawstat)
levene.test(siadd2$Vlog, group=siadd2$condadd, location="mean") #unequal
levene.test(siadd2$Vlog, group=siadd2$ID, location="mean") #unequal

#nlme
Form <- formula (Vlog ~ condadd)
si.gls<- gls(Form, data=siadd2)

#aov
si.aov <- aov(Form, data=siadd2)
library(lsmeans)
lsmeans(si.aov, pairwise~condadd, adjust="tukey")

#nlme model
si1.lme <- lme (Form, random = ~1|ID, method="REML", data=siadd2)

#si2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", data=siadd2)

si3.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|condadd), method="REML", data=siadd2)


#si4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|condadd), correlation=corAR1 (), method="REML", data=siadd2) #best

anova(si.gls, si1.lme, si3.lme) 

#best is si3.lme

library(multcomp)

summary(glht(si3.lme, covariate_average=TRUE, linfct=mcp(condadd="Tukey")))

library(car)
si.lm <-lm(Form, data=siadd2)
Anova(lm(Vlog~condadd, data=siadd2), type="3")


