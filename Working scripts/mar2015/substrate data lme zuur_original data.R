library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(nlme)

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=sub$Bin, treatment=sub$treatment, T=sub$T, A=sub$A, ID=sub$reptreat)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)

pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

source("summarySE.R")
dfc2 <- summarySE(sub, measurevar="CellsN", groupvars=c("reptreat"))

#boxplots

boxplot(CellsN~treatment, data=sub)
boxplot(CellsN~reptreat, data=sub)
boxplot (CellsN~Bin, data=sub)
boxplot (CellsN~T, data=sub)

library(lawstat)
levene.test(sub$CellsN, group=sub$reptreat, location="mean")
levene.test(sub$CellsN, group=sub$T, location="mean")
levene.test(sub$CellsN, group=sub$treatment, location="mean")
levene.test(sub$CellsN, group=sub$Bin, location="mean")


#The good approach in Zuur et al. 2009

#Step 1 Start with many explanatory variables as possible, use lm

M.lm <- lm(CellsN ~ treatment + Bin + T + treatment*Bin + treatment*T + T*Bin + treatment*T*Bin, data=sub)

op=par(mfrow=c(2,2))
plot(M.lm)

E=rstandard(M.lm)
boxplot(E ~ Bin, data=sub, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ treatment, data=sub, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ T, data=sub, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ reptreat, data=sub, axes=TRUE)
abline(0,0); axis=(2)

#Step 2. Fit with gls
Form <- formula (CellsN ~ treatment + Bin + T + treatment*Bin + treatment*T + T*Bin + treatment*T*Bin)
M.gls<- gls(Form, data=sub)

#Step 3 and 4 Choose a Variance structure, deciding on random factor is also part of this step and fit it

M1.lme <- lme(Form, random = ~1|reptreat, method="REML", data=sub)
M2.lme <- lme(Form, random = ~1|reptreat/Bin, method="REML", data=sub)
M3.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=sub)
M3A.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|Bin), method="REML", data=sub)
M4.lme <- lme(Form, random = ~1|reptreat/Bin, weights=varIdent(form=~1|reptreat), method="REML", data=sub)
M5.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), correlation= corAR1(),
              method="REML", data=sub)
M6.lme <- lme(Form, random = ~1|reptreat/Bin,  weights=varIdent(form=~1|reptreat), correlation= corAR1(),method="REML", data=sub)
M7.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
              correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=sub) 
M7A.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|Bin), 
              correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=sub) #best


#M8.lme <- lme(Form, random = ~1|reptreat/Bi,  weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=sub) 
M9.lme <- lme(Form, random = ~1|reptreat, correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=sub)
#M10.lme <- lme(Form, random = ~1|reptreat/Bin, correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=sub)

#choose M7A.lme as the most correct one
# Step 5 Compare models
anova(M.gls, M1.lme, M2.lme, M3.lme, M3A.lme, M4.lme, M5.lme, M6.lme, M7.lme, M9.lme)

# Step 6 Check if everything is okay
E2<-resid(M7A.lme,type="normalized")
F2<-fitted(M7A.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~treatment,data=sub,main="Treatment",ylab=MyYlab)
boxplot(E2~Bin,data=sub,main="Bin",ylab=MyYlab)
plot(x=sub$T,y=E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

E2<-resid(M7A.lme, type="normalized")

library(lattice)
xyplot (E2 ~ T| Bin*treatment, data=sub, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

library(mgcv)
M5.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs") + s(Bin, by=treatment, bs="fs") + 
                   ti(T, Bin, by=treatment, bs="fs", k=3), random = list(reptreat=~1),  
                correlation=corAR1 (form=~1|reptreat/treatment), 
                 method="REML", data=sub) 

summary(M5.gamm$gam) #no sig differences found
anova(M5.gamm$gam) # no sig differences found
plot(M5.gamm$gam) #things look linear, more support for using nlme
plot(M5.gamm$lme) # patterns seen on the residuals
summary(M5.gamm$lme)

#final model is M7A.lme
summary(M4.B)
anova(M4.B)


##BINS

#Bin A
BinA= subset (sub, Bin=='A')
expA=as.data.frame(data.table(cbind(treatment=BinA$treatment, T=BinA$T, ID=BinA$reptreat)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

levene.test(BinA$CellsN, group=BinA$reptreat, location="mean")
levene.test(BinA$CellsN, group=BinA$T, location="mean")
levene.test(BinA$CellsN, group=BinA$treatment, location="mean")


#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsN~treatment, data=BinA)
boxplot(CellsN~reptreat, data=BinA)
boxplot (CellsN~T, data=BinA)

#lm model
BinA.lm <- lm(CellsN ~treatment*T, data=BinA)

#barplots again
op=par(mfrow=c(2,2))
plot(BinA.lm)

BinA.E=rstandard(BinA.lm)
boxplot(BinA.E ~ treatment, data=BinA, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinA.E ~ T, data=BinA, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinA.E ~ reptreat, data=BinA, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (CellsN ~ treatment*T)
BinA.gls<- gls(Form, data=BinA)


#nlme model
BinA1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinA)

BinA2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinA)

BinA6.lme <- lme (Form, random = ~1|reptreat,  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinA)

BinA3.lme <- lme (CellsN ~ treatment*T, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinA) #best

BinA4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), 
                  correlation=corAR1 (), method="REML", data=BinA)

BinA5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinA) 

anova(BinA.gls, BinA1.lme, BinA2.lme, BinA3.lme, BinA4.lme, BinA5.lme, BinA6.lme)

summary(BinA3.lme)
anova(BinA3.lme)

#residuals
BinA.E2<-resid(BinA3.lme,type="normalized")
BinA.F2<-fitted(BinA3.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinA.F2,y=BinA.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinA.E2~treatment,data=BinA, main="Treatment",ylab=MyYlab)
plot(x=BinA$T,y=BinA.E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinA.E2 ~ T| treatment, data=BinA, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

#try gamm (gamm not working because of atomic vectors)
BinA.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs"),  random = list(reptreat=~1),  
                   weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), 
                   method="REML", data=BinA) 

summary(BinA.gamm$gam) 
anova(BinA.gamm$gam) 
plot(BinA.gamm$gam) 
plot(BinA.gamm$lme) 
summary(BinA.gamm$lme)


###BIN B

#BinB
BinB= subset (sub, Bin=='B')
expB=as.data.frame(data.table(cbind(treatment=BinB$treatment, T=BinB$T, A=BinB$A, ID=BinB$reptreat)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)
pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)


levene.test(BinB$CellsN, group=BinB$reptreat, location="mean")
levene.test(BinB$CellsN, group=BinB$T, location="mean")
levene.test(BinB$CellsN, group=BinB$treatment, location="mean")


#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsN~treatment, data=BinB)
boxplot(CellsN~reptreat, data=BinB)
boxplot (CellsN~T, data=BinB)

#lm model
BinB.lm <- lm(CellsN ~treatment*T, data=BinB)

#barplots again
op=par(mfrow=c(2,2))
plot(BinB.lm)

BinB.E=rstandard(BinB.lm)
boxplot(BinB.E ~ treatment, data=BinB, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinB.E ~ T, data=BinB, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinB.E ~ reptreat, data=BinB, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (CellsN ~ treatment*T)
BinB.gls<- gls(Form, data=BinB)


#nlme model
BinB1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinB)

BinB2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinB)

BinB3.lme <- lme (CellsN ~ treatment*T, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinB) #best

BinB4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), 
                  correlation=corAR1 (), method="REML", data=BinB) 

BinB5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinB) 

BinB6.lme <- lme (CellsN ~ treatment*T, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat/treatment), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinB) #best


anova(BinB.gls, BinB1.lme, BinB2.lme, BinB3.lme, BinB4.lme, BinB5.lme)

summary(BinB3.lme)
anova(BinB3.lme)

#residuals
BinB.E2<-resid(BinB3.lme,type="normalized")
BinB.F2<-fitted(BinB3.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinB.F2,y=BinB.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinB.E2~treatment,data=BinB, main="Treatment",ylab=MyYlab)
plot(x=BinB$T,y=BinB.E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinB.E2 ~ T| treatment, data=BinB, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

#try gamm (gamm not working)
BinB.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs"),  random = list(reptreat=~1),  
                   weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), 
                   method="REML", data=BinB) 

summary(BinB.gamm$gam) #Si significant
anova(BinB.gamm$gam) # Si significant
plot(BinB.gamm$gam) #things look linear, more support for using nlme
plot(BinB.gamm$lme) # patterns seen on the residuals
summary(BinB.gamm$lme)

##BIN C

#BinC

BinC= subset (sub, Bin=='C')
expC=as.data.frame(data.table(cbind(treatment=BinC$treatment, T=BinC$T, A=BinC$A, ID=BinC$reptreat)))
cor(expC, method = "spearman")

vif_func(in_frame=expC,thresh=5,trace=T)
pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

levene.test(BinC$CellsN, group=BinC$reptreat, location="mean")
levene.test(BinC$CellsN, group=BinC$T, location="mean")
levene.test(BinC$CellsN, group=BinC$treatment, location="mean")


#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsN~treatment, data=BinC)
boxplot(CellsN~reptreat, data=BinC)
boxplot (CellsN~T, data=BinC)

#lm model
BinC.lm <- lm(CellsN ~treatment*T, data=BinC)

#barplots again
op=par(mfrow=c(2,2))
plot(BinC.lm)

BinC.E=rstandard(BinC.lm)
boxplot(BinC.E ~ treatment, data=BinC, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinC.E ~ T, data=BinC, axes=TRUE)
abline(0,0); axis=(2)
boxplot(BinC.E ~ reptreat, data=BinC, axes=TRUE)
abline(0,0); axis=(2)
par(op)

#fit a gls
Form <- formula (CellsN ~ treatment*T)
BinC.gls<- gls(Form, data=BinC)


#nlme model
BinC1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinC)

BinC2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinC)

BinC3.lme <- lme (CellsN ~ treatment*T, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinC) #best

#BinC4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinC) 

BinC5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinC) 

anova(BinC.gls, BinC1.lme, BinC2.lme, BinC3.lme, BinC5.lme)

summary(BinC3.lme)
anova(BinC3.lme)

BinC6.lme <- lme (CellsN~treatment, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinC) 


#residuals
BinC.E2<-resid(BinC3.lme,type="normalized")
BinC.F2<-fitted(BinC3.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinC.F2,y=BinC.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinC.E2~treatment,data=BinC, main="Treatment",ylab=MyYlab)
plot(x=BinC$T,y=BinC.E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinC.E2 ~ T| treatment, data=BinC, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

#try gamm
BinC.gamm <- gamm (CellsN ~ s(T, by=treatment, bs="cs"),  random = list(reptreat=~1),  
                   weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), 
                   method="REML", data=BinC) 

summary(BinC.gamm$gam) #Si significant
anova(BinC.gamm$gam) # Si significant
plot(BinC.gamm$gam) #things look linear, more support for using nlme
plot(BinC.gamm$lme) # patterns seen on the residuals
summary(BinC.gamm$lme)

#plotting
grid.newpage()
text <- element_text(size = 18) #change the size of the axes
theme_set(theme_bw())

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Bin") { 
    value[value=="A"] <- "Bin A"
    value[value=="B"]   <- "Bin B"
    value[value=="C"] <- "Bin C"
  }
  return(value)
}

ggplot(data = sub, aes(x=T,y=CellsN, color=treatment, shape=treatment))+ 
  stat_smooth(method="loess", formula=y~x, size=2, se=TRUE)+ 
  facet_grid(.~Bin, labeller=mf_labeller)+ scale_colour_manual(values = c("lightcoral", "seagreen3", "steelblue2"),
                                                               breaks=c("Control", "Ge","Si"),
                                                               labels=c("Control", "dGe", "dSi")) +
  labs(list(x = "Time (sec)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.02),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))


BinA$treat2 <- factor(BinA$treatment, levels=c("Control", "Ge", "Si"), labels=c("Control", "dGe", "dSi"))


ggplot(data = BinA, aes(x=T,y=CellsN, color=treat2))+ 
  stat_smooth(method="loess", formula=y~x, size=2, se=TRUE)+ facet_grid (~treat2)+
  scale_colour_manual(values = c("lightcoral", "seagreen3", "steelblue2")) +
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.02),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=element_blank(), 
        legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_x_continuous (breaks=c(200, 400, 600)) 


