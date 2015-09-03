library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(plyr)
source("vif.R")
source ("AED.R")
source("lang.R")
source("summarySE.R")
source("lang.R")
s=mgcv:::s

write.table (binned, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binnedwithdist.csv", 
             sep=";", col.names=T, row.names=F)

Sumall <- ddply(binned, c("T", "bin", "cond"), summarise,
                N    = length(dist),
                mean = mean(dist, na.rm=TRUE),
                sum= sum(dist, na.rm=TRUE), 
                sd   = sd(dist, na.rm=TRUE),
                se   = sd / sqrt(N), na.rm=TRUE)

distall=Sumall

#subsetting per bin
BinA= subset (distall, bin=='binA')
BinB= subset (distall, bin=='binB')
BinC= subset (distall, bin=='binC')

#gam Bin A
##check for collinearity and correlation, this only applies to the explanatory variables!
expA=as.data.frame(data.table(cbind(cond=BinA$cond, T=BinA$T)))
cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))
boxplot(sum~cond, data=BinA)
boxplot(sum~T, data=BinA)

#levene
library(lawstat)
levene.test(BinA$sum, group=BinA$cond, location="mean") #unequal
levene.test(BinA$sum, group=BinA$T, location="mean") #unequal

#Bin A
BA <- gam (sum~s(T, by=cond, bs="fs"), method="REML", data = BinA)
BA1 <- gam (sum~s(T, by=cond, bs="fs", xt="cr"), method="REML", data = BinA) #best
BA2 <- gam (sum~s(T, by=cond, bs="fs", xt="cs"), method="REML", data = BinA) 
BA3 <- gamm (sum~s(T, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| cond), data = BinA) 
#BA4 <- gamm (sum~s(T, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| T), data = BinA) 
BA5 <- gamm (sum~s(T, by=cond, bs="fs", xt="cs"), method="REML", correlation=corAR1(), data = BinA) 



AIC(BA, BA1, BA2)

summary(BA1)
gam.check(BA1)
op=par(mfrow=c(2,1), mar=c(5.1,6.1,4.1,2.1))
plot(BA1, cex.lab=1.7, cex.axis=1.7)

#BIN B

##check for collinearity and correlation, this only applies to the explanatory variables!
expB=as.data.frame(data.table(cbind(cond=BinB$cond, T=BinB$T)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))
boxplot(sum~cond, data=BinB)
boxplot(sum~T, data=BinB)

#levene
library(lawstat)
levene.test(BinB$sum, group=BinB$cond, location="mean") #unequal
levene.test(BinB$sum, group=BinB$T, location="mean") #unequal


#Bin B
BB <- gam (sum~s(T, by=cond, bs="fs"), method="REML", data = BinB)
BB1 <- gam (sum~s(T, by=cond, bs="fs", xt="cr"), method="REML", data = BinB) #best
BB2 <- gam (sum~s(T, by=cond, bs="fs", xt="cs"), method="REML", data = BinB) 

AIC(BB, BB1, BB2)

summary(BB1)
gam.check(BB1)
op=par(mfrow=c(2,1), mar=c(5.1,6.1,4.1,2.1))
plot(BB1, cex.lab=1.7, cex.axis=1.7)

#BIN C

##check for collinearity and correlation, this only applies to the explanatory variables!
expC=as.data.frame(data.table(cbind(cond=BinC$cond, T=BinC$T)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))
boxplot(sum~cond, data=BinB)
boxplot(sum~T, data=BinB)

#levene
library(lawstat)
levene.test(BinC$sum, group=BinC$cond, location="mean") #unequal
levene.test(BinC$sum, group=BinC$T, location="mean") #unequal

#Bin C
BC <- gam (sum~s(T, by=cond, bs="fs"), method="REML", data = BinC) #best
BC1 <- gam (sum~s(T, by=cond, bs="fs", xt="cr"), method="REML", data = BinC) 
BC2 <- gam (sum~s(T, by=cond, bs="fs", xt="cs"), method="REML", data = BinC) 

AIC(BC, BC1, BC2)

summary(BC1)
gam.check(BC1)
op=par(mfrow=c(2,1), mar=c(5.1,6.1,4.1,2.1))
plot(BC1, cex.lab=1.7, cex.axis=1.7)

