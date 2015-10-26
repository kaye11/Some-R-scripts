
#summaries and removing data points with contribution of just 1 track
angs.sumall <- ddply(old_data, c("cond", "bin", "time"), summarise,
                     N    = length(angs),
                     ID   = length(unique(ID)),
                     mean = mean(angs, na.rm=TRUE),
                     sd   = sd(angs, na.rm=TRUE),
                     se   = sd / sqrt(N))


angs.sumall2 <- subset(angs.sumall, angs.sumall$ID>1 & angs.sumall$time>0,  )

ggplot(data=angs.sumall2, aes(x=time, y=mean, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=20, size=1) + facet_grid(~bin)

library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

source("AED.R")
source("vif.R")
source("tsDiagGamm.R")
source("lang.R")

#BEFORE DOING GAMM CHECK THE STRUCTURE OF DATA: ALL FACTORS SHOULD BE FACTORS AND TIME SHOULD BE NUMERIC:
#CONVERT IF NECESSARY!

#Bin A
BinA= subset (old_data, bin=='binA')
BinA <- BinA [! (BinA$cond=="dSi" & BinA$time=="360"),  ]
BinA <- BinA [! (BinA$cond=="dSi" & BinA$time=="600"),  ]
BinA <- BinA [!  BinA$time=="0",  ]
BinA <- na.omit(BinA)

qplot(timef, angs, color = cond, data = BinA,  geom = "boxplot") + facet_wrap(~cond, scales="free") 

expA=as.data.frame(data.table(cbind(cond=as.numeric(as.factor(BinA$cond)), T=as.numeric(BinA$time), ID=as.numeric(as.factor(BinA$ID)))))

cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, data=BinA)
boxplot(angs~ID, data=BinA)
boxplot (angs~time, data=BinA)

#levene
library(lawstat)
levene.test(BinA$angs, group=BinA$ID, location="mean") #unequal
levene.test(BinA$angs, group=BinA$time, location="mean") #unequal
levene.test(BinA$angs, group=BinA$cond, location="mean") #unequal

#fit a gls
Form <- formula (angs ~ cond*time)
BinA.gls<- gls(Form, BinA, na.action=na.omit)


#nlme model
BinA1.lme <- lme (Form, random = ~1|ID, method="REML", BinA, na.action=na.omit)

#BinA2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", BinA, na.action=na.omit)

#BinA3.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), correlation=corAR1 (form=~1|ID/cond), method="REML", BinA, na.action=na.omit)

BinA4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|time), 
                  correlation=corAR1 (form=~1|ID/cond), method="REML", BinA, na.action=na.omit) 

BinA5.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|cond), 
                  correlation=corAR1 (), method="REML", data=BinA, na.action=na.omit)#best is A5 

BinA6.lme <- lme (Form, random = ~1|ID,  weights=varExp(form=~fitted(.)), 
                  correlation=corAR1 (), method="REML", data=BinA, na.action=na.omit)


anova(BinA.gls, BinA1.lme, BinA4.lme, BinA5.lme, BinA6.lme)

summary(BinA5.lme)
anova(BinA5.lme)


#residuals
BinA.E2<-resid(BinA5.lme,type="normalized")
BinA.F2<-fitted(BinA5.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinA.F2,y=BinA.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinA.E2~cond,data=BinA, main="Treatment",ylab=MyYlab)
plot(x=BinA$time,y=BinA.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinA.E2 ~ time| cond, data=BinA, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})


#Bin B
BinB= subset (old_data, bin=='binB')
BinB <- BinB [!  BinB$time=="0",  ]
BinB <- na.omit(BinB)

qplot(timef, angs, color = cond, data = BinB,  geom = "boxplot") + facet_wrap(~cond, scales="free") 

expB=as.data.frame(data.table(cbind(cond=as.numeric(as.factor(BinB$cond)), T=as.numeric(BinB$time), ID=as.numeric(as.factor(BinB$ID)))))

cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(2,2))
boxplot(angs~cond, data=BinB)
boxplot(angs~ID, data=BinB)
boxplot (angs~time, data=BinB)

#levene
library(lawstat)
levene.test(BinB$angs, group=BinB$ID, location="mean") #unequal
levene.test(BinB$angs, group=BinB$time, location="mean") #unequal
levene.test(BinB$angs, group=BinB$cond, location="mean") #equal

#fit a gls
Form <- formula (angs ~ cond*time)
BinB.gls<- gls(Form, BinB, na.action=na.omit)


#nlme model
BinB1.lme <- lme (Form, random = ~1|ID, method="REML", BinB, na.action=na.omit)

#BinB2.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), method="REML", BinB, na.action=na.omit)

#BinB3.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|ID), correlation=corAR1 (form=~1|ID/cond), method="REML", BinB, na.action=na.omit)

#BinB4.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|time), correlation=corAR1 (form=~1|ID/cond), method="REML", BinB, na.action=na.omit) 

BinB5.lme <- lme (Form, random = ~1|ID,  weights=varIdent(form=~1|cond), 
                  correlation=corAR1 (), method="REML", data=BinB, na.action=na.omit)#best is B5 

BinB6.lme <- lme (Form, random = ~1|ID,  weights=varExp(form=~fitted(.)), 
                  correlation=corAR1 (), method="REML", data=BinB, na.action=na.omit)


anova(BinB.gls, BinB1.lme, BinB5.lme, BinB6.lme)

summary(BinB5.lme)
anova(BinB5.lme)


#residuals
BinB.E2<-resid(BinB5.lme,type="normalized")
BinB.F2<-fitted(BinB5.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinB.F2,y=BinB.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinB.E2~cond,data=BinB, main="Treatment",ylab=MyYlab)
plot(x=BinB$time,y=BinB.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinB.E2 ~ time| cond, data=BinB, ylab="Residuals", xlab="Time (sec)", 
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
resize.win(9,6)

#BinA fit

BinA.sum <- ddply(BinA, c("cond", "time"), summarise,
                  N    = length(angs),
                  ID   = length(unique(ID)),
                  mean = mean(angs, na.rm=TRUE),
                  sd   = sd(angs, na.rm=TRUE),
                  se   = sd / sqrt(N))


BinA.fit <- as.data.frame(predictSE.lme(BinA5.lme, BinA, se.fit = TRUE, level = 0,
                                        print.matrix = FALSE))

BinA.fit$upr <- BinA.fit$fit + (1.96 * BinA.fit$se)
BinA.fit$lwr <- BinA.fit$fit - (1.96 * BinA.fit$se)

BinA.fit.combdata <- cbind(BinA, BinA.fit)


#BinB fit

BinB.sum <- ddply(BinB, c("cond", "time"), summarise,
                  N    = length(angs),
                  ID   = length(unique(ID)),
                  mean = mean(angs, na.rm=TRUE),
                  sd   = sd(angs, na.rm=TRUE),
                  se   = sd / sqrt(N))


BinB.fit <- as.data.frame(predictSE.lme(BinB5.lme, BinB, se.fit = TRUE, level = 0,
                                        print.matrix = FALSE))

BinB.fit$upr <- BinB.fit$fit + (1.96 * BinB.fit$se)
BinB.fit$lwr <- BinB.fit$fit - (1.96 * BinB.fit$se)

BinB.fit.combdata <- cbind(BinB, BinB.fit)

#BinC fit

BinC.sum <- ddply(BinC, c("cond", "time"), summarise,
                  N    = length(angs),
                  ID   = length(unique(ID)),
                  mean = mean(angs, na.rm=TRUE),
                  sd   = sd(angs, na.rm=TRUE),
                  se   = sd / sqrt(N))


BinC.fit <- as.data.frame(predictSE.lme(BinC5.lme, BinC, se.fit = TRUE, level = 0,
                                        print.matrix = FALSE))

BinC.fit$upr <- BinC.fit$fit + (1.96 * BinC.fit$se)
BinC.fit$lwr <- BinC.fit$fit - (1.96 * BinC.fit$se)

BinC.fit.combdata <- cbind(BinC, BinC.fit)

allbins.fitdata = rbind (BinA.fit.combdata, BinB.fit.combdata, BinC.fit.combdata)
BinA.sum$bin="binA"
BinB.sum$bin="binB"
BinC.sum$bin="binC"

allbins.sum = rbind (BinA.sum, BinB.sum, BinC.sum)

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
  }
  return(value)
}


ggplot(data=allbins.sum, aes(x=time, y=mean, shape=cond, color=cond)) + geom_point(size=5)+ 
  facet_grid(.~bin, labeller=mf_labeller)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=20, size=1) +
  geom_smooth(data=allbins.fitdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=cond), stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Sine angle"))+  geom_hline(yintercept=0)+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 


ggplot(data=allbins.sum, aes(x=time, y=mean, shape=cond, color=cond)) + geom_point(size=5)+ 
  facet_grid(.~bin, labeller=mf_labeller)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=20, size=1) +
  geom_ribbon(data=allbins.fitdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=cond, linetype=NA), stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Sine angle"))+  geom_hline(yintercept=0)+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 

