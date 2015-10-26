library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
library(nlme)


dev.new(width=6, height=9)
source("resizewin.R")
resize.win(12,9)



#read data
count <- read.csv("D:/Karen's/PhD/R program/initial data/Data in csv/count_raw.csv", sep=";")

#correct time variable
count$T= count$time*60
count$T= as.factor(count$T)

#make a new factor
count$reptreatbin <- as.factor(paste(count$reptreat, count$Bin, sep = "-"))

#check initial plot
qplot(Bin,Cells, color = treatment, data = count,  geom = "boxplot") + facet_wrap(~treatment) 
qplot(T.factor,Cells, color = treatment, data = count,  geom = "boxplot") + facet_wrap(treatment~Bin, scales="free") 

#standardization

count$CellsS=NA
k=split(count, count$treatment)
countstd <- lapply(k, function (x) scale(x[,c("Cells")], center=T, scale=T))
count$CellsS=unsplit(countstd, count$treatment)

#baselining to 0 at time point 0
NT<-data.table(count, key=c("reptreatbin"))

t1=NT[,list(treatment=treatment, Bin=Bin, T=T, T=T, reptreat=reptreat, Cells=Cells, CellsS=CellsS,
            CellsBase=(CellsS-CellsS[1])), by=c("reptreatbin")]

countbase <- t1 #DATA IS NOW CALLED COUNTBASE

qplot(T,CellsBase, color = treatment, data = countbase,  geom = "boxplot") + facet_wrap(treatment~Bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

source("summarySE.R")
countbase.sum <- summarySE(countbase, measurevar="CellsBase", groupvars=c("T", "Bin", "treatment"))


ggplot(data=countbase.sum, aes(x=T, y=CellsBase, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=10, size=1) + facet_grid(Bin~treatment)

#######MODEL FITTING

#summaries

countbase.sum2 <- summarySE(countbase, measurevar="CellsBase", groupvars=c("reptreat"))

#check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")
exp=as.data.frame(data.table(cbind(bin=countbase$Bin, treatment=countbase$treatment, T=countbase$T, A=countbase$A, ID=countbase$reptreat)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)

pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist) 
#bin and ID (reptreat) has mild correlation

#boxplots
op=par(mfrow=c(2,2))

boxplot(CellsBase~treatment, data=countbase)
boxplot(CellsBase~reptreat, data=countbase)
boxplot (CellsBase~Bin, data=countbase)
boxplot (CellsBase~T, data=countbase)

#normality test
#by(countbase$CellsBase, countbase$reptreat, shapiro.test) 

shapiro.test(countbase$CellsBase) #not normal

library(lawstat)
levene.test(countbase$CellsBase, group=countbase$reptreat, location="mean") #significant
levene.test(countbase$CellsBase, group=countbase$T, location="mean") #significant
levene.test(countbase$CellsBase, group=countbase$treatment, location="mean") #significant
levene.test(countbase$CellsBase, group=countbase$Bin, location="mean") #significant

#fit with gls
Form <- formula (CellsBase ~ treatment*T*Bin)
M.gls<- gls(Form, data=countbase)

#Step 3 and 4 Choose a Variance structure, deciding on random factor is also part of this step and fit it

M1.lme <- lme(Form, random = ~1|reptreat, method="REML", data=countbase)
M2.lme <- lme(Form, random = ~1|reptreat/Bin, method="REML", data=countbase)
M3.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=countbase)
M4.lme <- lme(Form, random = ~1|reptreat/Bin, weights=varIdent(form=~1|reptreat), method="REML", data=countbase)
M5.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), correlation= corAR1(),
              method="REML", data=countbase)
M6.lme <- lme(Form, random = ~1|reptreat/Bin,  weights=varIdent(form=~1|reptreat), correlation= corAR1(),
              method="REML", data=countbase) #best correlation is reptreat/Bin

M7.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
              correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=countbase) # stick to this
#M8.lme <- lme(Form, random = ~1|reptreat/Bi,  weights=varIdent(form=~1|reptreat), correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=countbase)
#M9.lme <- lme(Form, random = ~1|reptreat/Bin,  weights=varPower(form=~fitted(.)) , correlation= corAR1(), method="REML", data=countbase)
#M10.lme <- lme(Form, random = ~1|reptreat/Bin,  weights=varComb(varIdent(form=~1|reptreat), varIdent (form=~1|T)), correlation= corAR1(), method="REML", data=countbase) 
#M11.lme <- lme(Form, random = ~1|reptreat/Bin,  weights=varIdent(form=~1|T), correlation= corAR1(), method="REML", data=countbase)

# Step 5 Compare models
anova(M.gls, M1.lme, M2.lme, M3.lme, M4.lme, M5.lme, M6.lme, M7.lme)

# Step 6 Check if everything is okay
E2<-resid(M6.lme,type="normalized")
F2<-fitted(M6.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~treatment,data=countbase,main="Treatment",ylab=MyYlab)
boxplot(E2~Bin,data=countbase,main="Bin",ylab=MyYlab)
plot(x=countbase$T,y=E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

library(lattice)
xyplot (E2 ~ T| Bin*treatment, data=countbase, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

##BINS

#Bin A
BinA= subset (countbase, Bin=='A')
expA=as.data.frame(data.table(cbind(treatment=BinA$treatment, T=BinA$T, ID=BinA$reptreat)))
cor(expA, method = "spearman")


vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

levene.test(BinA$CellsBase, group=BinA$reptreat, location="mean") #significant
levene.test(BinA$CellsBase, group=BinA$T, location="mean") # not significant
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
                  correlation=corAR1 (), method="REML", data=BinA) #best is A5 but could also choose BinA3

anova(BinA.gls, BinA1.lme, BinA2.lme, BinA3.lme, BinA5.lme)

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

xyplot (BinA.F2 ~ T| treatment, data=BinA, ylab="Fitted", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

###BIN B

#BinB
BinB= subset (countbase, Bin=='B')
expB=as.data.frame(data.table(cbind(treatment=BinB$treatment, T=BinB$T, A=BinB$A, ID=BinB$reptreat)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)
pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)


levene.test(BinB$CellsBase, group=BinB$reptreat, location="mean") #not significant
levene.test(BinB$CellsBase, group=BinB$T, location="mean") #not significant
levene.test(BinB$CellsBase, group=BinB$treatment, location="mean") #not significant

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

BinB3.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinB) #best

#BinB4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), correlation=corAR1 (), method="REML", data=BinB) 

BinB5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinB) 

anova(BinB.gls, BinB1.lme, BinB2.lme, BinB3.lme,BinB5.lme)

#best was BinB5.lme but anova between BinB1.lme, BinB3.lme and BinB5.lme were not significant

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

##BIN C

BinC= subset (countbase, Bin=='C')
expC=as.data.frame(data.table(cbind(treatment=BinC$treatment, T=BinC$T, A=BinC$A, ID=BinC$reptreat)))
cor(expC, method = "spearman")

vif_func(in_frame=expC,thresh=5,trace=T)
pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

levene.test(BinC$CellsBase, group=BinC$reptreat, location="mean") #significant
levene.test(BinC$CellsBase, group=BinC$T, location="mean")  #significant
levene.test(BinC$CellsBase, group=BinC$treatment, location="mean")  #not significant


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

BinC3.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=BinC) #best

#BinC4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), correlation=corAR1 (), method="REML", data=BinC) 

BinC5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1 (), method="REML", data=BinC) 

anova(BinC.gls, BinC1.lme, BinC2.lme, BinC3.lme, BinC5.lme)

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

resize.win(9,6)


#BinA fit
BinA.sum <- summarySE(BinA, measurevar="CellsBase", groupvars=c("T", "treatment"))

BinA.fit <- as.data.frame(predictSE.lme(BinA5.lme, BinA, se.fit = TRUE, level = 0,
                                  print.matrix = FALSE))

BinA.fit$upr <- BinA.fit$fit + (1.96 * BinA.fit$se)
BinA.fit$lwr <- BinA.fit$fit - (1.96 * BinA.fit$se)

BinA.fit.combdata <- cbind(BinA, BinA.fit)

BinA.fit.combdata$treatment2 <- factor(BinA.fit.combdata$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))
BinA.sum$treatment2 <- factor(BinA.sum$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))

BinA.plot = ggplot(data=BinA.fit.combdata, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+
  geom_smooth(data=BinA.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), method="lm", stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin A"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 




#Bin B
BinB.sum <- summarySE(BinB, measurevar="CellsBase", groupvars=c("T", "treatment"))

BinB.fit <- as.data.frame(predictSE.lme(BinB5.lme, BinB, se.fit = TRUE, level = 0,
                                        print.matrix = FALSE))

BinB.fit$upr <- BinB.fit$fit + (1.96 * BinB.fit$se)
BinB.fit$lwr <- BinB.fit$fit - (1.96 * BinB.fit$se)

BinB.fit.combdata <- cbind(BinB, BinB.fit)

BinB.fit.combdata$treatment2 <- factor(BinB.fit.combdata$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))
BinB.sum$treatment2 <- factor(BinB.sum$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))

BinB.plot = ggplot(data=BinB.fit.combdata, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+
  geom_smooth(data=BinB.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), method="lm", stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin B"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,0.2), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 




#Bin C

BinC.sum <- summarySE(BinC, measurevar="CellsBase", groupvars=c("T", "treatment"))

BinC.fit <- as.data.frame(predictSE.lme(BinC3.lme, BinC, se.fit = TRUE, level = 0,
                                        print.matrix = FALSE))

BinC.fit$upr <- BinC.fit$fit + (1.96 * BinC.fit$se)
BinC.fit$lwr <- BinC.fit$fit - (1.96 * BinC.fit$se)

BinC.fit.combdata <- cbind(BinC, BinC.fit)

BinC.fit.combdata$treatment2 <- factor(BinC.fit.combdata$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))
BinC.sum$treatment2 <- factor(BinC.sum$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))

BinC.plot = ggplot(data=BinC.fit.combdata, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+
  geom_smooth(data=BinC.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), method="lm", stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin C"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,0.2), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 



# Extracxt the legend from p1
legend = gtable_filter(ggplot_gtable(ggplot_build(BinA.plot)), "guide-box") 
grid.draw(legend)    # Make sure the legend has been extracted



grid.arrange(arrangeGrob(BinA.plot + theme (legend.position="none"),
             BinB.plot + theme (legend.position="none"),
             BinC.plot + theme (legend.position="none"),
             nrow=1,
             bottom=textGrob ("Time (s)", vjust=-1, hjust=0.2, gp=gpar(fontface="bold", cex=1.5)),
             left=textGrob ("Normalized cell count", vjust=2, rot=90, gp=gpar(fontface="bold", cex=1.5))),
             legend, 
             heights=unit.c(unit(1, "npc") - legend$height, legend$height), 
             ncol=1)
             




#with points and error bars

BinA.plot = ggplot(data=BinA.sum, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+ 
geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=10, size=1) + 
  geom_smooth(data=BinA.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), method="lm", stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin A"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 



BinB.plot = ggplot(data=BinB.sum, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+ 
geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=10, size=1) + 
  geom_smooth(data=BinB.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin B"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,0.2), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 



BinC.plot = ggplot(data=BinC.sum, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+ 
geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=10, size=1) + 
  geom_smooth(data=BinC.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), method="lm", stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin C"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank(), 
        axis.title.x=element_blank(),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,0.2), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 



# Extracxt the legend from p1
legend = gtable_filter(ggplot_gtable(ggplot_build(BinA.plot)), "guide-box") 
grid.draw(legend)    # Make sure the legend has been extracted



grid.arrange(arrangeGrob(BinA.plot + theme (legend.position="none"),
                         BinB.plot + theme (legend.position="none"),
                         BinC.plot + theme (legend.position="none"),
                         nrow=1,
                         bottom=textGrob ("Time (s)", vjust=-1, hjust=0.2, gp=gpar(fontface="bold", cex=1.5)),
                         left=textGrob ("Normalized cell count", vjust=2, rot=90, gp=gpar(fontface="bold", cex=1.5))),
             legend, 
             heights=unit.c(unit(1, "npc") - legend$height, legend$height), 
             ncol=1)



allbins.fitdata = rbind (BinA.fit.combdata, BinB.fit.combdata, BinC.fit.combdata)
BinA.sum$Bin="A"
BinB.sum$Bin="B"
BinC.sum$Bin="C"

allbins.sum = rbind (BinA.sum, BinB.sum, BinC.sum)

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Bin") { 
    value[value=="A"] <- "Bin A"
    value[value=="B"]   <- "Bin B"
    value[value=="C"] <- "Bin C"
  }
  return(value)
}

ggplot(data=allbins.sum, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+ 
  facet_grid(.~Bin, labeller=mf_labeller)+
  geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=20, size=1) +
  geom_smooth(data=allbins.fitdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), method="lm", stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 






ggplot(data=BinA.sum, aes(x=T, y=CellsBase, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=10, size=1) + 
  geom_ribbon(data=BinA.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, color=treatment, linetype=NA), stat="identity", alpha=0.3)
#geom_smooth(data=BinA.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), stat="identity", alpha=0.1) +


ggplot(data=BinA.sum, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsBase-sd, ymax=CellsBase+sd), width=12, size=1) + 
  geom_line(data=BinA.fit.combdata, aes(y=fit))+
  geom_ribbon(data=BinA.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2, linetype=NA), stat="identity", alpha=0.1) +
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin A"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 

ggplot(data=BinA.sum, aes(x=T, y=CellsBase, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsBase-sd, ymax=CellsBase+sd), width=12, size=1) + geom_point(data=BinA, aes(y=CellsBase))

