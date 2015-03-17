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
exp=as.data.frame(data.table(cbind(bin=binned$binn, cond=binned$condn, T=binned$T, A=binned$Acn)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(Vlog~cond, data=binned)
boxplot(Vlog~Acn, data=binned)
boxplot (Vlog~binn, data=binned)
boxplot (Vlog~T, data=binned)

#bestmodel WD
WDvel <- gamm (Vlog ~ s(T, by=cond, bs="cr", k=9) + s(binn, by=cond, bs="cr", k=3) + 
                 ti(T, binn, by=cond, k=3, bs="cr"), method="REML", 
               correlation = corAR1 (form = ~ 1|binn/Acn), weights= varIdent (form= ~1|condn), 
               random=list(Acn=~1), data=binned) #best

#Bin A
BinA= subset (binned, bin=='binA')
expA=as.data.frame(data.table(cbind(cond=BinA$cond, T=BinA$T, A=BinA$Acn, ID=BinA$Acn)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(Vlog~cond, data=BinA)
boxplot(Vlog~Acn, data=BinA)
boxplot (Vlog~T, data=BinA)

BA <- gamm (Vlog~s(T, by=cond), method="REML", data = BinA)
BA1 <- gamm (Vlog~s(T, by=cond, bs="cr", k=10), method="REML", data = BinA) 
BA2 <- gamm (Vlog~s(T, by=cond, bs="cs", k=10), method="REML", data = BinA) 
BA3 <- gamm (Vlog~te(T, by=cond), method="REML", data = BinA) 

anova(BA$lme, BA1$lme, BA2$lme, BA3$lme)

fBinA <- Vlog~s(T, by=cond, bs="cr", k=10)

BA4 <- gamm (fBinA, method="REML",  random=list(Acn=~1), data = BinA) 
BA5 <- gamm (fBinA, method="REML", correlation= corAR1 (form=~1|binn/Acn), data = BinA) 
BA6 <- gamm (fBinA, method="REML", correlation= corAR1 (form=~1|binn/Acn), weights = varIdent(form=~1| condn), 
             data = BinA) 
BA7 <- gamm (fBinA, method="REML", correlation= corAR1 (form=~1|binn/Acn), random=list(Acn=~1), 
             data = BinA) #BEST

anova(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme, BA6$lme, BA7$lme)

A <- gam (Vlog~s(T, by=cond), method="REML", data = BinA)
A1 <- gam (Vlog~s(T, by=cond, bs="cr"), method="REML", data = BinA)
A2 <- gam (Vlog~s(T, by=cond, bs="cs"), method="REML", data = BinA) 
A3 <- gam (Vlog~te(T, by=cond), method="REML", data = BinA) 
A4 <- gam (Vlog~s(T, by=cond, bs="cs", k=10) + s(Acn, bs="re"), method="REML", data = BinA) #best

AIC (A, A1, A2, A3, A4)

#BinB
BinB= subset (binned, bin=='binB')
expB=as.data.frame(data.table(cbind(cond=BinB$cond, T=BinB$T, A=BinB$Acn, ID=BinB$Acn)))
cor(expB, method = "spearman")


vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(Vlog~cond, data=BinB)
boxplot(Vlog~Acn, data=BinB)
boxplot (Vlog~T, data=BinB)

BB <- gamm (Vlog~s(T, by=cond), method="REML", data = BinB)
BB1 <- gamm (Vlog~s(T, by=cond, bs="cr", k=10), method="REML", data = BinB)
BB2 <- gamm (Vlog~s(T, by=cond, bs="cs"), method="REML", data = BinB) 
BB3 <- gamm (Vlog~te(T, by=cond), method="REML", data = BinB) 

anova(BB$lme, BB1$lme, BB2$lme, BB3$lme)

fBinB <- Vlog~s(T, by=cond, bs="cr", k=10) 

BB4 <- gamm (fBinB, method="REML",  random=list(Acn=~1), data = BinB)  
BB5 <- gamm (fBinB, method="REML", correlation= corAR1 (form=~1|binn/Acn), data = BinB) 
BB6 <- gamm (fBinB, method="REML", correlation= corAR1 (form=~1|binn/Acn), weights = varIdent(form=~1| condn), 
             data = BinB) 
BB7 <- gamm (fBinB, method="REML", correlation= corAR1 (form=~1|binn/Acn), random=list(Acn=~1), 
             data = BinB) #BEST

anova(BB$lme, BB1$lme, BB2$lme, BB3$lme, BB4$lme, BB5$lme, BB6$lme, BB7$lme)

B <- gam (Vlog~s(T, by=cond), method="REML", data = BinB)
B1 <- gam (Vlog~s(T, by=cond, bs="cr", k=3), method="REML", data = BinB)
B2 <- gam (Vlog~s(T, by=cond, bs="cs", k=3), method="REML", data = BinB) 
B3 <- gam (Vlog~te(T, by=cond), method="REML", data = BinB) 
B4 <- gam (Vlog~s(T, by=cond, bs="cs", k=) + s(Acn, bs="re"), method="REML", data = BinB) 

AIC (B, B1, B2, B3, B4)

#BinC

BinC= subset (binned, bin=='binC')
expC=as.data.frame(data.table(cbind(cond=BinC$cond, T=BinC$T, A=BinC$Acn, ID=BinC$Acn)))
cor(expC, method = "spearman")


vif_func(in_frame=expC,thresh=5,trace=T)

pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots

boxplot(Vlog~cond, data=BinC)
boxplot(Vlog~Acn, data=BinC)
boxplot (Vlog~T, data=BinC)

BC <- gamm (Vlog~s(T, by=cond), method="REML", data = BinC)
BC1 <- gamm (Vlog~s(T, by=cond, bs="cr", k=10), method="REML", data = BinC)
BC2 <- gamm (Vlog~s(T, by=cond, bs="cs"), method="REML", data = BinC) #best
BC3 <- gamm (Vlog~te(T, by=cond), method="REML", data = BinC) 

anova(BC$lme, BC1$lme, BC2$lme, BC3$lme)

fBinC <- Vlog~s(T, by=cond, bs="cr", k=10)

BC4 <- gamm (fBinC, method="REML",  random=list(Acn=~1), data = BinC) 
BC5 <- gamm (fBinC, method="REML", correlation= corAR1 (form=~1|binn/Acn), data = BinC) 
BC6 <- gamm (fBinC, method="REML", correlation= corAR1 (form=~1|binn/Acn), weights = varIdent(form=~1| condn), 
             data = BinC) 
BC7 <- gamm (fBinC, method="REML", correlation= corAR1 (form=~1|binn/Acn), random=list(Acn=~1), 
             data = BinC) #BEST

anova(BC$lme, BC1$lme, BC2$lme, BC3$lme, BC4$lme, BC5$lme, BC6$lme, BC7$lme)

C <- gam (Vlog~s(T, by=cond), method="REML", data = BinC)
C1 <- gam (Vlog~s(T, by=cond, bs="cr", k=3), method="REML", data = BinC)
C2 <- gam (Vlog~s(T, by=cond, bs="cs", k=3), method="REML", data = BinC) 
C3 <- gam (Vlog~te(T, by=cond), method="REML", data = BinC) 
C4 <- gam (Vlog~s(T, by=cond, bs="cs", k=3) + s(Acn, bs="re"), method="REML", data = BinC) 

AIC (C, C1, C2, C3, C4)

M.gee1 <- geeglm (Vlog ~ cond + bin + time, data=binned, id=Acn, corstr="ar1")