
pre <- read.csv("D:/Karen's/PhD/R program/initial data/Data in csv/prenew.csv", sep=";")

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
pre$T= pre$time*60
pre$T.factor= as.factor(pre$T)

#check initial plot
qplot(T.factor,Cells, color = treatment, data = pre,  geom = "boxplot") + facet_wrap(~treatment, scales="free") 


#standardization

pre$CellsS=NA
k=split(pre, pre$treatment)
prestd <- lapply(k, function (x) scale(x[,c("Cells")], center=T, scale=T))
pre$CellsS=unsplit(prestd, pre$treatment)

qplot(T.factor,CellsS, color = treatment, data = pre,  geom = "boxplot") + facet_wrap(~treatment, scales="free") 

#baselining to 0 at time point 0
NT<-data.table(pre, key=c("reptreat"))

t1=NT[,list(treatment=treatment, T=T, T.factor=T.factor, reptreat=reptreat, Cells=Cells, CellsS=CellsS,
            CellsBase=(CellsS-CellsS[1])), by=c("reptreat")]

prebase <- t1 #DATA IS NOW CALLED preBASE

qplot(T.factor,CellsBase, color = treatment, data = prebase,  geom = "boxplot") + facet_wrap(~treatment) +
  stat_smooth (method="lm", formula=y~x, size=1, aes(group=1))

#summary
source("summarySE.R")

prebase.sum <- summarySE(prebase, measurevar="CellsBase", groupvars=c( "T", "treatment"))

exp=as.data.frame(data.table(cbind(treatment=prebase$treatment, T=prebase$T, ID=prebase$reptreat)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)

pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

library(lawstat)
levene.test(prebase$CellsBase, group=prebase$reptreat, location="mean") #significant
levene.test(prebase$CellsBase, group=prebase$T, location="mean") # significant
levene.test(prebase$CellsBase, group=prebase$treatment, location="mean") #significant


#boxplots
op=par(mfrow=c(2,2))
boxplot(CellsBase~treatment, data=prebase)
boxplot(CellsBase~reptreat, data=prebase)
boxplot (CellsBase~T, data=prebase)

#fit a gls
Form <- formula (CellsBase ~ treatment*T)
prebase.gls<- gls(Form, data=prebase)


#nlme model
prebase1.lme <- lme (Form, random = ~1|reptreat, method="REML", data=prebase)

prebase2.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=prebase)

prebase21.lme <- lme (Form, random = ~1|treatment/reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=prebase) 

prebase3.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), 
                  correlation=corAR1 (form=~1|reptreat/treatment), method="REML", data=prebase) 

#prebase4.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|T), correlation=corAR1 (), method="REML", data=prebase) 

prebase5.lme <- lme (Form, random = ~1|reptreat,  weights=varIdent(form=~1|treatment), 
                  correlation=corAR1(), method="REML", data=prebase) #best is A5 

anova(prebase.gls, prebase1.lme, prebase2.lme, prebase21.lme, prebase3.lme, prebase5.lme) 
#choose prebase3.lme

summary(prebase3.lme)
anova(prebase3.lme)

#residuals
prebase.E2<-resid(prebase3.lme,type="normalized")
prebase.F2<-fitted(prebase5.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=prebase.F2,y=prebase.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(prebase.E2~treatment,data=prebase, main="Treatment",ylab=MyYlab)
plot(x=prebase$T,y=prebase.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

library(lattice)
xyplot (prebase.E2 ~ T| treatment, data=prebase, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})


#let's plot this!

grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw()) 
library (AICcmodavg)

source("resizewin.R")
resize.win(9, 6)

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="treatment") { 
    value[value=="control"] <- "Control"
    value[value=="t1"]   <- "0.088 nmoles/bead"
    value[value=="t2"] <- "1.4 nmoles/bead"
    value[value=="t3"] <- "2.23 nmoles/bead"
  }
  return(value)
}


#prebase fit
prebase.sum <- summarySE(prebase, measurevar="CellsBase", groupvars=c("T", "treatment"))

prebase.fit <- as.data.frame(predictSE.lme(prebase3.lme, prebase, se.fit = TRUE, level = 0,
                                        print.matrix = FALSE))

prebase.fit$upr <- prebase.fit$fit + (1.96 * prebase.fit$se)
prebase.fit$lwr <- prebase.fit$fit - (1.96 * prebase.fit$se)

prebase.fit.combdata <- cbind(prebase, prebase.fit)


ggplot(data=prebase.sum, aes(x=T, y=CellsBase, shape=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsBase-se, ymax=CellsBase+se), width=30, size=1) + 
  geom_smooth(data=prebase.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr), method="lm", stat="identity", alpha=0.3)+ 
  facet_grid(.~treatment, labeller=mf_labeller)+ 
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = element_text(size=15), strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 

library(multcomp)
summary(glht(prebase3.lme, linfct=mcp(treatment="Tukey", covariate_average=TRUE, interaction_average=TRUE)))
