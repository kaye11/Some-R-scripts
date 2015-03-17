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
SpeedsumCond2 <- summarySE(binned, measurevar="V", groupvars=c("cond"))

#levene
library(lawstat)
levene.test(binned$Vlog, group=binned$ID, location="mean") #unequal
levene.test(binned$Vlog, group=binned$time, location="mean") #unequal
levene.test(binned$Vlog, group=binned$cond, location="mean") #unequal
levene.test(binned$Vlog, group=binned$bin, location="mean")
levene.test(binned$Vlog, group=binned$T, location="mean")

#gam formula
SG <- gamm (Vlog ~ s(T, by=cond, bs="fs"), data=binned)
SG1 <- gamm (Vlog ~ s(T, by=cond, bs="fs") + s(bin, by=cond, bs="fs"), data=binned)
SG2 <- gamm (Vlog ~ s(T, by=cond, bs="fs", xt="cr") + s(bin, by=cond, bs="fs", xt="cr") + ti (T, bin, by=cond, bs="fs", xt="cr"), data=binned) #best
SG2.1 <- gamm (Vlog ~ s(T, by=cond, bs="fs", xt="cs") + s(bin, by=cond, bs="fs", xt="cs") + ti (T, bin, by=cond, bs="fs", xt="cs"), data=binned)


#put correlations
Form <- formula (Vlog ~ s(T, by=cond, bs="fs", xt="cr") + s(bin, by=cond, bs="fs", xt="cr") + ti (T, bin, by=cond, bs="fs", xt="cr"))

SG3 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), data=binned)
SG4 <- gamm (Form, correlation = corAR1 (), data=binned) #cannot allocate vector of size 2.5Gb

#put random factor

SG5 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), random=list(ID=~1), data=binned)
SG6 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), random=list(ID=~1|bin), data=binned) #same with SG5

#put variance structures

SG7 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), random=list(ID=~1), weights= varIdent (form= ~1|cond), data=binned) #best
#SG7.1 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), random=list(ID=~1), weights= varIdent (form= ~1|cond*bin), data=binned) #no convergence
#SG7.2 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), random=list(ID=~1), weights= varIdent (form= ~1|cond*T), data=binned) #no convergence
#SG8 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), random=list(ID=~1), weights= varIdent (form= ~1|T), data=binned) # no convergence
#SG9 <- gamm (Form, correlation = corAR1 (form = ~ 1|bin/ID), random=list(ID=~1), weights= varIdent (form= ~1|bin), data=binned) # no convergence

anova(SG$lme, SG1$lme, SG2$lme, SG2.1$lme, SG3$lme, SG5$lme, SG6$lme, SG7$lme)

#best model is SG7 AIC= 25232.25

gam.check (SG7$gam)

ggplot(data = binned, aes(x=T,y=V, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7) , size=2, se=TRUE) + facet_grid(.~bin)

ggplot(data = binned, aes(x=T,y=V, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7) , size=2, se=TRUE) 

op=par(mfrow=c(2,2))
plot(SG7$gam)
plot(SG7$lme)

#try nlme
SG.lme <- lme (Vlog ~ cond*bin*time, correlation = corAR1 (), random = ~1|ID, weights= varIdent (form= ~1|cond), data=binned)
SG1.lme <- lme (Vlog ~ cond*bin*time, correlation = corAR1 (form = ~ 1|bin/ID), random = ~1|ID, weights= varIdent (form= ~1|cond), data=binned)

summary(SG7$gam) 
anova(SG7$gam) 
plot(SG7$lme) 
summary(SG7$lme)

##plotting
grid.newpage()
text <- element_text(size = 18, face="bold") #change the size of the axes
theme_set(theme_bw())

mf_labeller2 <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
  }
  return(value)
}

ggplot(data = binned, aes(x=T,y=Vlog, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7), size=2, se=TRUE)+  
  facet_grid(.~bin, labeller=mf_labeller2)+  scale_colour_manual(values = c("lightcoral", "steelblue2")) +
  labs(list(x = "Time (s)", y = "Mean speed (µm/s)"))+ labs (color="Experimental Condition")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#extract estimates of gam
summary_model <- summary(SG7$gam)
summary_model$p.table
summary_model$s.table

p_table <- data.frame(summary_model$p.table)
p_table <- within(p_table, {lci <- Estimate - qnorm(0.975) * Std..Error
                            uci <- Estimate + qnorm(0.975) * Std..Error})
p_table


summary(glht(SG7$lme, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BA8$lme, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BB8$lme, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BC8$lme, covariate_average=TRUE, interaction_average=TRUE))

summary(glht(SG7$gam, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BA8$gam, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BB8$gam, covariate_average=TRUE, interaction_average=TRUE))
summary(glht(BC8$gam, covariate_average=TRUE, interaction_average=TRUE))

### define coefficients of linear function directly
K <- diag(length(coef(BA8$gam)))[-1,]
rownames(K) <- names(coef(BA8$gam))[-1]
summary(glht(BA8$gam,covariate_average=TRUE, linfct=K))

L <- diag(length(coef(BB8$gam)))[-1,]
rownames(L) <- names(coef(BB8$gam))[-1]
summary(glht(BB8$gam,covariate_average=TRUE, linfct=L))

M <- diag(length(coef(BC8$gam)))[-1,]
rownames(M) <- names(coef(BC8$gam))[-1]
summary(glht(BC8$gam,covariate_average=TRUE, linfct=M))

