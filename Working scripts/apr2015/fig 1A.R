library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(nlme)

#COUNT DATA FIG 1A
#summaries
source("summarySE.R")
countsum <- summarySE(count, measurevar="count", groupvars=c("rep"), na.rm=T)

#check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(day=count$day, rep=count$rep)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)

pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))

boxplot(count~day, data=count)
boxplot(count~rep, data=count)


#normality test
by(count$count, count$rep, shapiro.test) 

shapiro.test(count$count)

#homogeneity of variance
#  the null hypothesis is that all populations variances are equal; 
# the alternative hypothesis is that at least two of them differ.

library(lawstat)
levene.test(count$count, group=count$rep, location="mean")
levene.test(count$count, group=count$day, location="mean")


#lm
M.lm <- lm (count~day, data=count)
M.lm1 <- lme (count~day, random = ~1|rep, data=count, na.action=na.pass) #does not work
M.lm2 <- lme (count~day, random = ~1|rep, data=count, na.action=na.omit)
M.lm3 <- lme (count~day, random = ~1|rep, data=count, na.action=na.exclude) #same as M.lm3
M.lm4 <- lme (count~day, random = ~1|rep, data=count, na.action=na.omit, weights=varIdent(form=~1|day))

summary(M.lm4)
anova(M.lm4)
summary(glht(M.lm4, linfct=mcp(day="Tukey")))

#----------------------------------#

#SI DATA FIG 1A

#summaries
source("summarySE.R")
sisum <- summarySE(si, measurevar="Si", groupvars=c("replicate"), na.rm=T)

#check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
expsi=as.data.frame(data.table(cbind(day=si$day, replicate=si$replicate)))
cor(expsi, method = "spearman")

vif_func(in_frame=expsi,thresh=5,trace=T)

pairs(expsi, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))

boxplot(Si~day, data=si)
boxplot(Si~replicate, data=si)


#normality test
by(si$Si, si$replicate, shapiro.test) 

shapiro.test(si$Si)

#homogeneity of variance
#  the null hypothesis is that all populations variances are equal; 
# the alternative hypothesis is that at least two of them differ.

library(lawstat)
levene.test(si$Si, group=si$replicate, location="mean")
levene.test(si$Si, group=si$day, location="mean")


#lm
M.lmS <- lm (Si~day, data=si)
M.lm1S <- lme (Si~day, random = ~1|replicate, data=si, na.action=na.pass) 
M.lm2S <- lme (Si~day, random = ~1|replicate, data=si, na.action=na.omit)
M.lm3S <- lme (Si~day, random = ~1|replicate, data=si, na.action=na.exclude) #same as M.lm1-2
M.lm4S <- lme (Si~day, random = ~1|replicate, data=si, na.action=na.omit, weights=varIdent(form=~1|day)) #singular matrix


anova(M.lm1S, M.lm2S, M.lm3S )

summary(M.lm2S)
anova(M.lm2S)
summary(glht(M.lm2S, linfct=mcp(day="Tukey")))

