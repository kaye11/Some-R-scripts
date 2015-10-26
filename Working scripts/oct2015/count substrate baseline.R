
#substrate <- read.csv("D:/Karen's/PhD/R program/initial data/Data in csv/sub_original.csv", sep=";")
#substrate <- read.csv("D:/Karen's/PhD/R program/initial data/Data in csv/sub_2_datafromexp.csv", sep=";")
substrate <- read.csv("D:/Karen's/PhD/R program/initial data/Data in csv/sub_3_datafromexp.csv", sep=";") # data that was used


library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(nlme)

source("vif.R")
source("AED.R")
source("lang.R")

dev.new(width=6, height=9)
source("resizewin.R")
resize.win(9,6)

#correct time variable
substrate$T= substrate$time*60
substrate$T.factor= as.factor(substrate$T)

#make a new factor
substrate$reptreatbin <- as.factor(paste(substrate$reptreat, substrate$Bin, sep = "-"))

#check initial plot
qplot(Bin,Cells, color = treatment, data = substrate,  geom = "boxplot") + facet_wrap(~treatment) 
qplot(T.factor,Cells, color = treatment, data = substrate,  geom = "boxplot") + facet_wrap(treatment~Bin, scales="free") 


#standardization

substrate$CellsS=NA
k=split(substrate, substrate$treatment)
substratestd <- lapply(k, function (x) scale(x[,c("Cells")], center=T, scale=T))
substrate$CellsS=unsplit(substratestd, substrate$treatment)

#baselining to 0 at time point 0
NT<-data.table(substrate, key=c("reptreatbin"))

t1=NT[,list(treatment=treatment, Bin=Bin, T=T, T.factor=T.factor, reptreat=reptreat, Cells=Cells, CellsS=CellsS,
            CellsBase=(CellsS-CellsS[1])), by=c("reptreatbin")]

substratebase <- t1 #DATA IS NOW CALLED substrateBASE

qplot(T.factor,CellsBase, color = treatment, data = substratebase,  geom = "boxplot") + facet_wrap(treatment~Bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))


#summary
source("summarySE.R")

substratebase.sum <- summarySE(substratebase, measurevar="CellsBase", groupvars=c("Bin", "T", "treatment"))

ggplot(data=substratebase.sum, aes(x=T, y=CellsBase, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=10, size=1) + facet_grid(Bin~treatment)

#Bin A
BinA= subset (substratebase, Bin=='A')
expA=as.data.frame(data.table(cbind(treatment=BinA$treatment, T=BinA$T, ID=BinA$reptreat)))
cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

library(lawstat)
levene.test(BinA$CellsBase, group=BinA$reptreat, location="mean") #significant
levene.test(BinA$CellsBase, group=BinA$T, location="mean") # significant
levene.test(BinA$CellsBase, group=BinA$treatment, location="mean") #significant


#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsBase~treatment, data=BinA)
boxplot(CellsBase~reptreat, data=BinA)
boxplot (CellsBase~T, data=BinA)

#fit a gls
Form <- formula (CellsBase ~ treatment*T)
BinA.gls<- gls(Form, data=BinA)


#nlme model
BinA1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinA)

BinA2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinA)

BinA21.lme <- lme (Form, random = ~1|treatment/reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinA) 

BinA3.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinA) 

#BinA4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), correlation=corAR1 (), method="REML", data=BinA) 

BinA5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1(), method="REML", data=BinA) #best is A5 

anova(BinA.gls, BinA1.lme, BinA2.lme, BinA21.lme, BinA3.lme, BinA5.lme) #you can choose bet BinA3 and A5, it does not matter. 
#choose BinA5 for consistency's sake. 

summary(BinA5.lme)
anova(BinA5.lme)


#residuals
BinA.E2<-resid(BinA5.lme,type="normalized")
BinA.F2<-fitted(BinA5.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinA.F2,y=BinA.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinA.E2~treatment,data=BinA, main="Treatment",ylab=MyYlab)
plot(x=BinA$T,y=BinA.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinA.E2 ~ T| treatment, data=BinA, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})


#Bin B

#Bin B
BinB= subset (substratebase, Bin=='B')
expB=as.data.frame(data.table(cbind(treatment=BinB$treatment, T=BinB$T, ID=BinB$reptreat)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)


levene.test(BinB$CellsBase, group=BinB$reptreat, location="mean") #significant
levene.test(BinB$CellsBase, group=BinB$T, location="mean") #significant
levene.test(BinB$CellsBase, group=BinB$treatment, location="mean") #significant


#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsBase~treatment, data=BinB)
boxplot(CellsBase~reptreat, data=BinB)
boxplot (CellsBase~T, data=BinB)

#fit a gls
Form <- formula (CellsBase ~ treatment*T)
BinB.gls<- gls(Form, data=BinB)


#nlme model
BinB1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinB)

BinB2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinB)

BinB21.lme <- lme (Form, random = ~1|treatment/reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinB) 

BinB3.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinB) 

#BinB4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), correlation=corAR1 (), method="REML", data=BinB) 

BinB5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinB) #best is A5 but could also choose BinB3

anova(BinB.gls, BinB1.lme, BinB2.lme, BinB21.lme, BinB3.lme, BinB5.lme)

summary(BinB5.lme)
anova(BinB5.lme)



#residuals
BinB.E2<-resid(BinB5.lme,type="normalized")
BinB.F2<-fitted(BinB5.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinB.F2,y=BinB.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinB.E2~treatment,data=BinB, main="Treatment",ylab=MyYlab)
plot(x=BinB$T,y=BinB.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinB.E2 ~ T| treatment, data=BinB, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})



#Bin C

#Bin B
BinC= subset (substratebase, Bin=='C')
expC=as.data.frame(data.table(cbind(treatment=BinC$treatment, T=BinC$T, ID=BinC$reptreat)))
cor(expC, method = "spearman")

vif_func(in_frame=expC,thresh=5,trace=T)

pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)


levene.test(BinC$CellsBase, group=BinC$reptreat, location="mean") #significant
levene.test(BinC$CellsBase, group=BinC$T, location="mean") #significant
levene.test(BinC$CellsBase, group=BinC$treatment, location="mean") #not significant


#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsBase~treatment, data=BinC)
boxplot(CellsBase~reptreat, data=BinC)
boxplot (CellsBase~T, data=BinC)

#fit a gls
Form <- formula (CellsBase ~ treatment*T)
BinC.gls<- gls(Form, data=BinC)


#nlme model
BinC1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=BinC)

BinC2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinC)

BinC21.lme <- lme (Form, random = ~1|treatment/reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=BinC) 

BinC3.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinC) 

#BinC4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), correlation=corAR1 (), method="REML", data=BinC) 

BinC5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinC) #best is A5 but could also choose BinC3

anova(BinC.gls, BinC1.lme, BinC2.lme, BinC21.lme, BinC3.lme, BinC5.lme)

summary(BinC5.lme)
anova(BinC5.lme)



#residuals
BinC.E2<-resid(BinC5.lme,type="normalized")
BinC.F2<-fitted(BinC5.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinC.F2,y=BinC.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinC.E2~treatment,data=BinC, main="Treatment",ylab=MyYlab)
plot(x=BinC$T,y=BinC.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinC.E2 ~ T| treatment, data=BinC, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})


#let's plot this!

grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw()) 
library (AICcmodavg)

resize.win(9, 6)



#BinA fit
BinA.sum <- summarySE(BinA, measurevar="CellsBase", groupvars=c("T", "treatment"))

BinA.fit <- as.data.frame(predictSE.lme(BinA5.lme, BinA, se.fit = TRUE, level = 0,
                                        print.matrix = FALSE))

BinA.fit$upr <- BinA.fit$fit + (1.96 * BinA.fit$se)
BinA.fit$lwr <- BinA.fit$fit - (1.96 * BinA.fit$se)

BinA.fit.combdata <- cbind(BinA, BinA.fit)

BinA.fit.combdata$treatment2 <- factor(BinA.fit.combdata$treatment, levels=c("Control", "Si", "Ge"), labels=c("Control", "dSi", "dGe"))
BinA.sum$treatment2 <- factor(BinA.sum$treatment, levels=c("Control", "Si", "Ge"), labels=c("Control", "dSi", "dGe"))

ggplot(data=BinA.sum, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=10, size=1) + 
  geom_smooth(data=BinA.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), method="lm", stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2", dGe="seagreen3"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_manual (values = c(Control="lightcoral", dSi="steelblue2", dGe="seagreen3"), name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin A", alpha=0.2))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 









