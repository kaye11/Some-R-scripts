#summaries and removing data points with contribution of just 1 track

speedsumall <- ddply(old_data, c("cond", "bin", "time"), summarise,
                     N    = length(V),
                     ID   = length(unique(ID)),
                     mean = mean(V, na.rm=TRUE),
                     sd   = sd(V, na.rm=TRUE),
                     se   = sd / sqrt(N))


speedsumall2 <- subset(speedsumall, speedsumall$ID>1 & speedsumall$time>0,  )



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

#Bin A
BinA= subset (old_data, bin=='binA')
BinA <- BinA [! (BinA$cond=="dSi" & BinA$time=="360"),  ]
BinA <- BinA [! (BinA$cond=="dSi" & BinA$time=="600"),  ]
BinA <- BinA [! BinA$time=="0",  ]

qplot(timef, Vlog, color = cond, data = BinA,  geom = "boxplot") + facet_wrap(~cond, scales="free") 


expA=as.data.frame(data.table(cbind(cond=as.numeric(as.factor(BinA$cond)), T=as.numeric(BinA$time), ID=as.numeric(as.factor(BinA$ID)))))

cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#summaries
SpeedBinA <- summarySE(BinA, measurevar="V", groupvars=c("cond", "time"))
SpeedBinB <- summarySE(BinB, measurevar="V", groupvars=c("cond", "time"))
SpeedBinC <- summarySE(BinC, measurevar="V", groupvars=c("cond","time"))


#boxplots
op=par(mfrow=c(2,2))
boxplot(Vlog~cond, data=BinA)
boxplot(Vlog~ID, data=BinA)
boxplot (Vlog~time, data=BinA)

#levene
library(lawstat)
levene.test(BinA$Vlog, group=BinA$ID, location="mean") #unequal
levene.test(BinA$Vlog, group=BinA$time, location="mean") #unequal
levene.test(BinA$Vlog, group=BinA$cond, location="mean")

#gamm
BA <- gamm (Vlog~s(time, by=cond, bs="fs"), method="REML", data = BinA)
BA1 <- gamm (Vlog~s(time, by=cond, bs="fs", xt="cr"), method="REML", data = BinA) #best
BA2 <- gamm (Vlog~s(time, by=cond, bs="fs", xt="cs"), method="REML", data = BinA) 

anova(BA$lme, BA1$lme, BA2$lme)

#make random factor and correlations
fBinA <- Vlog~s(time, by=cond, bs="fs", xt="cr")

BA3 <- gamm (fBinA, method="REML",  random=list(ID=~1), data = BinA) 
BA4 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), data = BinA) #BEST
BA5 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (), data = BinA) #same with BA4

anova(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme)

#make variance structures
#BA6 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| time), data = BinA) #no convergence

#BA7 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| ID), data = BinA) #no convergence

BA8 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), 
             weights = varIdent(form=~1| cond), data = BinA) 

BA9 <- gamm (fBinA, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varExp(form=~fitted(.)), data = BinA) 
#BA9 worked but kind of weirdly


anova(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme, BA8$lme)

AIC(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme, BA8$lme, BA9$lme)

with(BinA, tsDiagGamm(BA9, timevar=time, observed=Vlog))

#best model is BA8

gam.check (BA8$gam)

op=par(mfrow=c(2,1), mar=c(5.1,6.1,4.1,2.1))
plot(BA8$gam, cex.lab=1.7, cex.axis=1.7)
plot(BB8$gam, cex.lab=1.7, cex.axis=1.7)
plot(BC8$gam, cex.lab=1.7, cex.axis=1.7)

summary(BA8$gam)
anova(BA8$gam)

plot(BA8$lme)


#Bin B
BinB= subset (old_data, bin=='binB')
BinB <- BinB [! BinB$time=="0",  ]

qplot(timef, Vlog, color = cond, data = BinB,  geom = "boxplot") + facet_wrap(~cond, scales="free") 


expB=as.data.frame(data.table(cbind(cond=as.numeric(as.factor(BinB$cond)), T=as.numeric(BinB$time), ID=as.numeric(as.factor(BinB$ID)))))

cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#summaries
SpeedBinA <- summarySE(BinB, measurevar="V", groupvars=c("cond", "time"))
SpeedBinB <- summarySE(BinB, measurevar="V", groupvars=c("cond", "time"))
SpeedBinC <- summarySE(BinC, measurevar="V", groupvars=c("cond","time"))

#boxplots
op=par(mfrow=c(2,2))
boxplot(Vlog~cond, data=BinB)
boxplot(Vlog~ID, data=BinB)
boxplot (Vlog~time, data=BinB)

#levene
library(lawstat)
levene.test(BinB$Vlog, group=BinB$ID, location="mean") #unequal
levene.test(BinB$Vlog, group=BinB$time, location="mean") #unequal
levene.test(BinB$Vlog, group=BinB$cond, location="mean")

#gamm
BB <- gamm (Vlog~s(time, by=cond, bs="fs"), method="REML", data = BinB)
BB1 <- gamm (Vlog~s(time, by=cond, bs="fs", xt="cr"), method="REML", data = BinB) #best
BB2 <- gamm (Vlog~s(time, by=cond, bs="fs", xt="cs"), method="REML", data = BinB) 

anova(BB$lme, BB1$lme, BB2$lme)

#make random factor and correlations
fBinB <- Vlog~s(time, by=cond, bs="fs", xt="cr")

BB3 <- gamm (fBinB, method="REML",  random=list(ID=~1), data = BinB) 
BB4 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), data = BinB) #BEST
BB5 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (), data = BinB) #same with BB4

anova(BB$lme, BB1$lme, BB2$lme, BB3$lme, BB4$lme, BB5$lme)

#make variance structures
#BB6 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| time), data = BinB) #no convergence

#BB7 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| ID), data = BinB) #no convergence

BB8 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), 
             weights = varIdent(form=~1| cond), data = BinB) 

BB9 <- gamm (fBinB, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varExp(form=~fitted(.)), data = BinB) 
#BB9 worked but kind of weirdly


anova(BB$lme, BB1$lme, BB2$lme, BB3$lme, BB4$lme, BB5$lme, BB8$lme)
AIC(BB$lme, BB1$lme, BB2$lme, BB3$lme, BB4$lme, BB5$lme, BB8$lme, BB9$lme)

with(BinB, tsDiagGamm(BB8, timevar=time, observed=Vlog))

#best model is BB8

gam.check (BB8$gam)

op=par(mfrow=c(2,1), mar=c(5.1,6.1,4.1,2.1))
plot(BB8$gam, cex.lab=1.7, cex.axis=1.7)

summary(BB8$gam)
anova(BB8$gam)

plot(BB8$lme)

#Bin C
BinC= subset (old_data, bin=='binC')
BinC <- BinC [! BinC$time=="0",  ]

qplot(timef, Vlog, color = cond, data = BinC,  geom = "boxplot") + facet_wrap(~cond, scales="free") 

expC=as.data.frame(data.table(cbind(cond=as.numeric(as.factor(BinC$cond)), T=as.numeric(BinC$time), ID=as.numeric(as.factor(BinC$ID)))))

cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)

pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#summaries
SpeedBinC <- summarySE(BinC, measurevar="V", groupvars=c("cond","time"))


#boxplots
op=par(mfrow=c(2,2))
boxplot(Vlog~cond, data=BinC)
boxplot(Vlog~ID, data=BinC)
boxplot (Vlog~time, data=BinC)

#levene
library(lawstat)
levene.test(BinC$Vlog, group=BinC$ID, location="mean") #unequal
levene.test(BinC$Vlog, group=BinC$time, location="mean") #unequal
levene.test(BinC$Vlog, group=BinC$cond, location="mean") #unequal

#gamm
BC <- gamm (Vlog~s(time, by=cond, bs="fs"), method="REML", data = BinC)
BC1 <- gamm (Vlog~s(time, by=cond, bs="fs", xt="cr"), method="REML", data = BinC) #best
BC2 <- gamm (Vlog~s(time, by=cond, bs="fs", xt="cs"), method="REML", data = BinC) 

anova(BC$lme, BC1$lme, BC2$lme)

#make random factor and correlations
fBinC <- Vlog~s(time, by=cond, bs="fs", xt="cr")

BC3 <- gamm (fBinC, method="REML",  random=list(ID=~1), data = BinC) 
BC4 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), data = BinC) #BEST
BC5 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (), data = BinC) #same with BC4

anova(BC$lme, BC1$lme, BC2$lme, BC3$lme, BC4$lme, BC5$lme)

#make variance structures
#BC6 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| time), data = BinC) #no convergence

#BC7 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varIdent(form=~1| ID), data = BinC) #no convergence

BC8 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), 
             weights = varIdent(form=~1| cond), data = BinC) 

BC9 <- gamm (fBinC, method="REML", random=list(ID=~1), correlation= corAR1 (form=~1|cond/ID), weights = varExp(form=~fitted(.)), data = BinC) 
#BC9 worked but kind of weirdly


anova(BC$lme, BC1$lme, BC2$lme, BC3$lme, BC4$lme, BC5$lme, BC8$lme)
AIC(BC$lme, BC1$lme, BC2$lme, BC3$lme, BC4$lme, BC5$lme, BC8$lme, BC9$lme)

with(BinC, tsDiagGamm(BC8, timevar=time, observed=Vlog))

#best model is BC8

gam.check (BC8$gam)

op=par(mfrow=c(2,1), mar=c(5.1,6.1,4.1,2.1))
plot(BC8$gam, cex.lab=1.7, cex.axis=1.7)

summary(BC8$gam)
anova(BC8$gam)

plot(BC8$lme)

#plotting
grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw()) 
source("resizewin.R")

resize.win(9,6)


mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
  }
  return(value)
}



ggplot(data=speedsumall2, aes(x=time, y=mean, shape=cond, color=cond)) + geom_point(size=5)+
         geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=20, size=1) + facet_grid(~bin, labeller=mf_labeller)+
         scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
         scale_shape_discrete (name="Treatment") +
         scale_fill_discrete(name="Treatment") +
         labs(list(x = "Time (s)", y = "Mean cell speed (µm/s)"))+
         theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
               axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
               plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
               strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
               panel.grid.major = element_blank(),panel.margin.y = unit(1, "lines"), 
               panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 
       
#Bin A sum
BinA.sum <- subset(speedsumall2, bin=="binA")
 
ggplot(data=BinA.sum, aes(x=time, y=mean, shape=cond, color=cond)) + geom_point(size=5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=20, size=1) + 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") +
  labs(list(x = "Time (s)", y = "Mean cell speed (µm/s)",  title="Bin A"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),panel.margin.y = unit(1, "lines"), 
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 


