#binspa data

binspa$Vlog=log(binspa$V+1)
levels(binspa$An)[levels(binspa$An)=="0"] <- "5000"
binspa$An=as.numeric(binspa$A)

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
source ("lang.R")
exp=as.data.frame(data.table(cbind(bin=binspa$bin, cond=binspa$cond, time=binspa$time, A=binspa$A)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(Vlog~cond, data=binspa)
boxplot(Vlog~A, data=binspa)
boxplot (Vlog~bin, data=binspa)
boxplot (Vlog~time, data=binspa)

W21 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), random=list(A=~1, binn=~1),
             weights= varComb (varIdent (form= ~1|binn), varPower (form= ~binn|condn)),
             data=binspa)

f1 <- (Vlog ~ s(time, by=cond, bs="cr") + s(binn, by=cond, bs="cr", k=3) + ti(time, binn, by=cond, k=3, bs="cr"))

G2 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), random=list(A=~1), data=binspa) #best

W12 <- gamm (f1, method="REML", correlation = corAR1 (form = ~ 1|cond/A), random=list(A=~1, binn=~1),
             weights= varPower (form= ~binn|condn), data=binspa)

       