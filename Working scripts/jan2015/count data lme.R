library(nlme)
library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(data.table)

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

#model without variance and correlation structures
M1 <- lme(CellsN ~ treatment*Bin*T, random= ~1| reptreat/Bin, method="REML", data=count)
plot(M1)

#model with correlation structure
M2 <- lme(CellsN ~ treatment*Bin*T, random= ~1| reptreat/Bin, correlation=corAR1 (), method="REML", data=count)
plot(M2)

#model with variance structure
M3 <- lme(CellsN ~ treatment*Bin*T, random= ~1| reptreat/Bin, correlation=corAR1 (), weights=varIdent(form=~1|reptreat),
          method="REML", data=count)
plot (M3)

anova(M1, M2, M3)

summary(M3)

#check residuals
E2 <- resid(M3, type = "normalized")
F2 <- fitted(M3)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
MyYlab <- "Residuals"
plot(x = F2, y = E2, xlab = "Fitted values", ylab = MyYlab)
boxplot(E2 ~ treatment, data = count,
        main = "treatment", ylab = MyYlab)
boxplot(E2 ~ Bin, data = count,
        main = "Bin", ylab = MyYlab)
E=rstandard(M3.lm)
plot(x = count$T, y = E, ylab = MyYlab,
     main = "Time", xlab = "Time (sec)")

