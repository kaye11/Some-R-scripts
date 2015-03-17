library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

#Bin A
BinA= subset (binned, bin=='binA')
expA=as.data.frame(data.table(cbind(cond=BinA$cond, T=BinA$T, ID=BinA$ID)))
cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#summaries
SpeedBinA <- summarySE(BinA, measurevar="V", groupvars=c("cond"))
SpeedBinB <- summarySE(BinB, measurevar="V", groupvars=c("cond"))
SpeedBinC <- summarySE(BinC, measurevar="V", groupvars=c("cond"))

speedbins=as.data.frame(rbind(SpeedBinA, SpeedBinB, SpeedBinC))

#boxplots
op=par(mfrow=c(2,2))
boxplot(Vlog~cond, data=BinA)
boxplot(Vlog~ID, data=BinA)
boxplot (Vlog~T, data=BinA)

#levene
library(lawstat)
levene.test(BinA$Vlog, group=BinA$ID, location="mean") #unequal
levene.test(BinA$Vlog, group=BinA$time, location="mean") #unequal
levene.test(BinA$Vlog, group=BinA$cond, location="mean")
levene.test(BinA$Vlog, group=BinA$T, location="mean") # unequal

#gamm
BA <- gamm (Vlog~s(T, by=cond, bs="fs"), method="REML", data = BinA)
BA1 <- gamm (Vlog~s(T, by=cond, bs="fs", xt="cr"), method="REML", data = BinA) #best
BA2 <- gamm (Vlog~s(T, by=cond, bs="fs", xt="cs"), method="REML", data = BinA) 

anova(BA$lme, BA1$lme, BA2$lme)

#make random factor and correlations
fBinA <- Vlog~s(T, by=cond, bs="fs", xt="cr")

BA3 <- gamm (fBinA, method="REML",  random=list(ID=~1), data = BinA) 
BA4 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), data = BinA) #BEST
BA5 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (), data = BinA) #same with BA4

#make variance structures
#BA6 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| T), data = BinA) #no convergence

#BA7 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| ID), data = BinA) #no convergence

BA8 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), 
             weights = varIdent(form=~1| cond), data = BinA) #BEST

anova(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme, BA8$lme)

#best model is BA8

gam.check (BA8$gam)

op=par(mfrow=c(2,1))
plot(BA8$gam)

summary(BA8$gam)
anova(BA8$gam)

plot(BA8$lme)


#extract estimates of gam
summary_modelA <- summary(BA8$gam)
summary_modelA$p.table
summary_modelA$s.table

p_table.A <- data.frame(summary_modelA$p.table)
p_table.A <- within(p_table, {lci <- Estimate - qnorm(0.975) * Std..Error
                            uci <- Estimate + qnorm(0.975) * Std..Error})
p_table.A


plot(BA8$lme)
summary(BA8$lme)

##BIN B
BinB= subset (binned, bin=='binB')
expB=as.data.frame(data.table(cbind(cond=BinB$cond, T=BinB$T, ID=BinB$ID)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(Vlog~cond, data=BinB)
boxplot(Vlog~ID, data=BinB)
boxplot (Vlog~T, data=BinB)

#levene
library(lawstat)
levene.test(BinB$Vlog, group=BinB$ID, location="mean") #unequal
levene.test(BinB$Vlog, group=BinB$time, location="mean") #unequal
levene.test(BinB$Vlog, group=BinB$cond, location="mean")
levene.test(BinB$Vlog, group=BinB$T, location="mean") # unequal

#gamm
BB <- gamm (Vlog~s(T, by=cond, bs="fs"), method="REML", data = BinB)
BB1 <- gamm (Vlog~s(T, by=cond, bs="fs", xt="cr"), method="REML", data = BinB) #best
BB2 <- gamm (Vlog~s(T, by=cond, bs="fs", xt="cs"), method="REML", data = BinB) 

anova(BB$lme, BB1$lme, BB2$lme)

#make random factor and correlations
fBinB <- Vlog~s(T, by=cond, bs="fs", xt="cr")

BB3 <- gamm (fBinB, method="REML",  random=list(ID=~1), data = BinB) 
BB4 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), data = BinB) #BEST
BB5 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (), data = BinB) #same with BB4

#make variance structures
#BB6 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| T), data = BinB) #no convergence

#BB7 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| ID), data = BinB) #no convergence

BB8 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), 
             weights = varIdent(form=~1| cond), data = BinB) #BEST

anova(BB$lme, BB1$lme, BB2$lme, BB3$lme, BB4$lme, BB5$lme, BB8$lme)

#best model is BB8

gam.check (BB8$gam)

op=par(mfrow=c(2,1))
plot(BB8$gam)

summary(BB8$gam)
anova(BB8$gam)


#extract estimates of gam
summary_modelB <- summary(BB8$gam)
summary_modelB$p.table
summary_modelB$s.table

p_table.B <- data.frame(summary_modelB$p.table)
p_table.B <- within(p_table, {lci <- Estimate - qnorm(0.975) * Std..Error
                              uci <- Estimate + qnorm(0.975) * Std..Error})
p_table.B


plot(BB8$lme)
summary(BB8$lme)


##BIN C

BinC= subset (binned, bin=='binC')
expC=as.data.frame(data.table(cbind(cond=BinC$cond, T=BinC$T, ID=BinC$ID)))
cor(expC, method = "spearman")

vif_func(in_frame=expC,thresh=5,trace=T)

pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(Vlog~cond, data=BinC)
boxplot(Vlog~ID, data=BinC)
boxplot (Vlog~T, data=BinC)

#levene
library(lawstat)
levene.test(BinC$Vlog, group=BinC$ID, location="mean") #unequal
levene.test(BinC$Vlog, group=BinC$time, location="mean") #unequal
levene.test(BinC$Vlog, group=BinC$cond, location="mean") #uneuqual
levene.test(BinC$Vlog, group=BinC$T, location="mean") # unequal

#gamm
BC <- gamm (Vlog~s(T, by=cond, bs="fs"), method="REML", data = BinC)
BC1 <- gamm (Vlog~s(T, by=cond, bs="fs", xt="cr"), method="REML", data = BinC) #best
BC2 <- gamm (Vlog~s(T, by=cond, bs="fs", xt="cs"), method="REML", data = BinC) 

anova(BC$lme, BC1$lme, BC2$lme)

#make random factor and correlations
fBinC <- Vlog~s(T, by=cond, bs="fs", xt="cr")

BC3 <- gamm (fBinC, method="REML",  random=list(ID=~1), data = BinC) 
BC4 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), data = BinC) #BEST
BC5 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (), data = BinC) #same with BC4

#make variance structures
#BC6 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| T), data = BinC) #no convergence

#BC7 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| ID), data = BinC) #no convergence

BC8 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), 
             weights = varIdent(form=~1| cond), data = BinC) #BEST

anova(BC$lme, BC1$lme, BC2$lme, BC3$lme, BC4$lme, BC5$lme, BC8$lme)

#best model is BC8

gam.check (BC8$gam)

op=par(mfrow=c(2,1))
plot(BC8$gam)

summary(BC8$gam)
anova(BC8$gam)


#extract estimates of gam
summary_modelC <- summary(BC8$gam)
summary_modelC$p.table
summary_modelC$s.table

p_table.C <- data.frame(summary_modelC$p.table)
p_table.C <- within(p_table, {lci <- Estimate - qnorm(0.975) * Std..Error
                              uci <- Estimate + qnorm(0.975) * Std..Error})
p_table.C


plot(BC8$lme)
summary(BC8$lme)

summary(BC8$gam) 
anova(BC8$gam) 
plot(BC8$lme) 
summary(BC8$lme)

#ex: exp(2.324645)-1
