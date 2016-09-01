# Combine A and cond
control$ID <- paste(control$A, control$cond, sep = "-")
si$ID <- paste(si$A, si$cond, sep = "-")

#put alox coordinate positions
control$alox.X=455
control$alox.Y=377
si$alox.X=452
si$alox.Y=345

#subsetting
controlBinA=control[control$bin=="binA", ]
controlBinB=control[control$bin=="binB", ]
controlBinC=control[control$bin=="binC", ]

siBinA=control[si$bin=="binA", ]
siBinB=control[si$bin=="binB", ]
siBinC=control[si$bin=="binC", ]

library(plyr)
control.sort <- arrange (control, A, T)
si.sort <- arrange (si, A, T)

old_data <- rbind (control.sort, si.sort)

#distance

old_data$dist=sqrt(((old_data$alox.X-old_data$X)^2)+((old_data$alox.Y-old_data$Y)^2))

distall <- ddply(old_data, c("time", "bin", "cond"), summarise,
                 N    = length(dist),
                 mean = mean(dist, na.rm=TRUE),
                 sum= sum(dist, na.rm=TRUE), 
                 sd   = sd(dist, na.rm=TRUE),
                 se   = sd / sqrt(N))

qplot(x=time, y=sum, data=distall)+geom_line()+facet_grid(bin~cond, scales="free")

distall <- distall [! distall$time=="0",  ]

distall$sum_mm <- distall$sum/1000

qplot(x=time, y=sum_mm, data=distall)+geom_line()+facet_grid(bin~cond, scales="free")

distall$cond=as.factor(distall$cond)


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


##check for collinearity and correlation, this only applies to the explanatory variables!
expA=as.data.frame(data.table(cbind(cond=BinA$cond, T=BinA$time)))
cor(expA, method = "spearman")

vif_func(in_frame=expA,thresh=5,trace=T)


pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))
boxplot(sum_mm~cond, data=BinA)
boxplot(sum_mm~time, data=BinA)

#levene
library(lawstat)
levene.test(BinA$sum_mm, group=BinA$cond, location="mean") #equal
levene.test(BinA$sum_mm, group=BinA$time, location="mean") #unequal

#Bin A
BA <- gam (sum_mm~s(time, by=cond, bs="fs"), method="REML", data = BinA)
BA1 <- gam (sum_mm~s(time, by=cond, bs="fs", xt="cr"), method="REML", data = BinA) 
BA2 <- gam (sum_mm~s(time, by=cond, bs="fs", xt="cs", k=5), method="REML", data = BinA) #best
BA3 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| cond), data = BinA) 
#BA4 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| time), data = BinA) 
BA5 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", correlation=corAR1(), data = BinA) 

#gamm doesnt improve anything, BA1 is better

AIC(BA, BA1, BA2, BA3$lme, BA5$lme, BA.gls, BA.lm)

summary(BA.lm)
anova(BA.lm)

#try linear model

BA.gls <- gls (sum_mm~time*cond, method="REML", data=BinA)
BA.lm <- lm (sum_mm~time*cond,  data=BinA) #lm is best try the fits
#BA.lme <- lme (sum_mm~time*cond, method="REML", weights=varIdent(form=~1| time), data=BinA )
#CHOOSE SIMPLE BA.LM 

#residuals
BinA.E2<-resid(BA.lm,type="response")
BinA.F2<-fitted(BA.lm)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinA.F2,y=BinA.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinA.E2~cond,data=BinA, main="cond",ylab=MyYlab)
plot(x=BinA$time,y=BinA.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinA.E2 ~ time| cond, data=BinA, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

library(lsmeans)
lsmeans(BA.lm, pairwise~cond*time, adjust="tukey")

#gam Bin B
##check for collinearity and correlation, this only applies to the explanatory variables!
expB=as.data.frame(data.table(cbind(cond=BinB$cond, T=BinB$time)))
cor(expB, method = "spearman")

vif_func(in_frame=expB,thresh=5,trace=T)

pairs(expB, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))
boxplot(sum_mm~cond, data=BinB)
boxplot(sum_mm~time, data=BinB)

#levene
library(lawstat)
levene.test(BinB$sum_mm, group=BinB$cond, location="mean") #unequal
levene.test(BinB$sum_mm, group=BinB$time, location="mean") #unequal

#Bin B
BB <- gam (sum_mm~s(time, by=cond, bs="fs"), method="REML", data = BinB)
BB1 <- gam (sum_mm~s(time, by=cond, bs="fs", xt="cr"), method="REML", data = BinB) 
BB2 <- gam (sum_mm~s(time, by=cond, bs="fs", xt="cs", k=5), method="REML", data = BinB) #best
BB3 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| cond), data = BinB) 
#BB4 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| time), data = BinB) 
BB5 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", correlation=corAR1(), data = BinB) 

#gamm doesnt improve anything, BB1 is better

AIC(BB, BB1, BB2, BB3$lme, BB5$lme, BB.gls, BB.lm) 
#best was BB5

#try linear model

BB.gls <- gls (sum_mm~time*cond, method="REML", data=BinB)
BB.lm <- lm (sum_mm~time*cond,  data=BinB) #lm is best try the fits
#BB.lme <- lme (sum_mm~time*cond, method="REML", weights=varIdent(form=~1| time), data=BinB )

#BB.lm was the best

#residuals
BinB.E2<-resid(BB.lm,type="response")
BinB.F2<-fitted(BB.lm)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinB.F2,y=BinB.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinB.E2~cond,data=BinB, main="cond",ylab=MyYlab)
plot(x=BinB$time,y=BinB.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinB.E2 ~ time| cond, data=BinB, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})


lsmeans(BB.lm, pairwise~cond*time, adjust="tukey")

#Bin C
##check for collinearity and correlation, this only applies to the explanatory variables!
expC=as.data.frame(data.table(cbind(cond=BinC$cond, T=BinC$time)))
cor(expC, method = "spearman")

vif_func(in_frame=expC,thresh=5,trace=T)

pairs(expC, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

#boxplots
op=par(mfrow=c(1,2))
boxplot(sum_mm~cond, data=BinC)
boxplot(sum_mm~time, data=BinC)

#levene
library(lawstat)
levene.test(BinC$sum_mm, group=BinC$cond, location="mean") #equal
levene.test(BinC$sum_mm, group=BinC$time, location="mean") #unequal

#Bin C
BC <- gam (sum_mm~s(time, by=cond, bs="fs"), method="REML", data = BinC)
BC1 <- gam (sum_mm~s(time, by=cond, bs="fs", xt="cr"), method="REML", data = BinC) 
BC2 <- gam (sum_mm~s(time, by=cond, bs="fs", xt="cs", k=5), method="REML", data = BinC) #best
BC3 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| cond), data = BinC) 
#BC4 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", weights=varIdent(form=~1| time), data = BinC) 
BC5 <- gamm (sum_mm~s(time, by=cond, bs="fs", xt="cs"), method="REML", correlation=corAR1(), data = BinC) 

AIC(BC, BC1, BC2, BC3$lme, BC5$lme, BC.gls, BC.lm) 
#best was BC5

#try linear model

BC.gls <- gls (sum_mm~time*cond, method="REML", data=BinC)
BC.lm <- lm (sum_mm~time*cond,  data=BinC) #lm is best try the fits
#BC.lme <- lme (sum_mm~time*cond, method="REML", weights=varIdent(form=~1| time), data=BinC )

AIC(BC, BC1, BC2, BC3$lme, BC5$lme, BC.gls, BC.lm) 

#BC.lm was the best

#residuals
BinC.E2<-resid(BC.lm,type="response")
BinC.F2<-fitted(BC.lm)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=BinC.F2,y=BinC.E2,xlab="Fitted values",ylab=MyYlab)
boxplot(BinC.E2~cond,data=BinC, main="cond",ylab=MyYlab)
plot(x=BinC$time,y=BinC.E2,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

xyplot (BinC.E2 ~ time| cond, data=BinC, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

lsmeans(BC.lm, pairwise~cond*time, adjust="tukey")



#BinA fit linear model

BinA.sum <- subset(distall, distall$bin=="binA", )

BinA.fit <- as.data.frame(predict(BA.lm, BinA, se.fit = TRUE, level = 0))

BinA.fit$upr <- BinA.fit$fit + (1.96 * BinA.fit$se)
BinA.fit$lwr <- BinA.fit$fit - (1.96 * BinA.fit$se)

BinA.fit.combdata <- cbind(BinA, BinA.fit)

ggplot(data=BinA.sum, aes(x=time, y=sum_mm, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_smooth(data=BinA.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=cond), stat="identity", alpha=0.1)

#check gam predictions

BinA.fit.gam <- as.data.frame(predict(BA1, BinA, se.fit = TRUE, level = 0))

BinA.fit.gam$upr <- BinA.fit.gam$fit + (1.96 * BinA.fit.gam$se)
BinA.fit.gam$lwr <- BinA.fit.gam$fit - (1.96 * BinA.fit.gam$se)

BinA.fit.combdata.gam <- cbind(BinA, BinA.fit.gam)

ggplot(data=BinA.sum, aes(x=time, y=sum_mm, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_smooth(data=BinA.fit.combdata.gam, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=cond), stat="identity", alpha=0.1)

#linear model fits are better. stick to linear model


#BinB fit linear model

BinB.sum <- subset(distall, distall$bin=="binB", )

BinB.fit <- as.data.frame(predict(BB.lm, BinB, se.fit = TRUE, level = 0))

BinB.fit$upr <- BinB.fit$fit + (1.96 * BinB.fit$se)
BinB.fit$lwr <- BinB.fit$fit - (1.96 * BinB.fit$se)

BinB.fit.combdata <- cbind(BinB, BinB.fit)

ggplot(data=BinB.sum, aes(x=time, y=sum_mm, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_smooth(data=BinB.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=cond), stat="identity", alpha=0.1)


#BinC fit linear model

BinC.sum <- subset(distall, distall$bin=="binC", )

BinC.fit <- as.data.frame(predict(BC.lm, BinC, se.fit = TRUE, level = 0))

BinC.fit$upr <- BinC.fit$fit + (1.96 * BinC.fit$se)
BinC.fit$lwr <- BinC.fit$fit - (1.96 * BinC.fit$se)

BinC.fit.combdata <- cbind(BinC, BinC.fit)

ggplot(data=BinC.sum, aes(x=time, y=sum_mm, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_smooth(data=BinC.fit.combdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=cond), stat="identity", alpha=0.1)

#plotting
grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw()) 

source("resizewin.R")
resize.win(6,9)

allbins.fitdata = rbind (BinA.fit.combdata, BinB.fit.combdata, BinC.fit.combdata)


allbins.sum = distall

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
  }
  return(value)
}


a=ggplot(data=allbins.sum, aes(x=time, y=sum_mm, shape=cond, color=cond)) + geom_point(size=5)+ 
  facet_grid(bin~., labeller=mf_labeller, scales="free")+
  geom_smooth(data=allbins.fitdata, size=1,  aes(y=fit, ymin=lwr, ymax=upr, fill=cond), stat="identity", alpha=0.1)+ 
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Sum distance from the bead (mm)"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position=c(0.5,-0.25), legend.direction="horizontal",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),panel.margin.y = unit(1, "lines"), 
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,3.5,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 


