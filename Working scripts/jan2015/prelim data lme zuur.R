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

exp=as.data.frame(data.table(cbind(treatment=pre$treatment, T=pre$T, ID=pre$reptreat)))
cor(exp, method = "spearman")

vif_func(in_frame=exp,thresh=5,trace=T)

pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

op=par(mfrow=c(2,2))
boxplot(CellsN~treatment, data=pre)
boxplot(CellsN~reptreat, data=pre)
boxplot (CellsN~T, data=pre)

#homogeneity of variances checking

library(lawstat)
levene.test(pre$CellsN, group=pre$reptreat, location="mean")
levene.test(pre$CellsN, group=pre$T, location="mean")
levene.test(pre$CellsN, group=pre$treatment, location="mean")

#nlme
#1. lm
M.lm=lm(CellsN~treatment*T, data=pre)

op=par(mfrow=c(2,2))
plot(M.lm)

E=rstandard(M.lm)
boxplot(E ~ treatment, data=pre, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ T, data=pre, axes=TRUE)
abline(0,0); axis=(2)
boxplot(E ~ reptreat, data=pre, axes=TRUE)
abline(0,0); axis=(2)

#2. gls
Form <-formula(CellsN~treatment*T)
M.gls <-  gls (Form, data=pre)

#Step 3 and 4 Choose a Variance structure, deciding on random factor is also part of this step and fit it

M1.lme <- lme(Form, random = ~1|reptreat, method="REML", data=pre)
M2.lme <- lme(Form, random = ~1|reptreat/T, method="REML", data=pre)
M3.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), method="REML", data=pre)
#M4.lme <- lme(Form, random = ~1|reptreat, weights=varIdent(form=~1|reptreat/T), method="REML", data=pre)
M5.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), correlation= corAR1(),
              method="REML", data=pre) #best
M6.lme <- lme(Form, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), correlation= corAR1(form=~1|reptreat/treatment), 
              method="REML", data=pre) #same with M5.lme

M7.lme <- lme(Form, random = ~1|reptreat, correlation= corAR1(form=~1|reptreat/treatment), 
              method="REML", data=pre) 

#step 5 anova
anova(M.gls, M1.lme, M2.lme, M3.lme, M5.lme, M6.lme)

# Step 6 Check if everything is okay
E2<-resid(M6.lme,type="normalized")
F2<-fitted(M6.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~treatment,data=pre,main="Treatment",ylab=MyYlab)
boxplot(E2~reptreat,data=pre,main="Bin",ylab=MyYlab)
plot(x=pre$T,y=E,main="Time",ylab=MyYlab,xlab="Time (sec)")
par(op)

library(lattice)
xyplot (E2 ~ T| treatment, data=pre, ylab="Residuals", xlab="Time (sec)", 
        panel=function(x,y){
          panel.grid(h=-1, v= 2)
          panel.points(x,y,col=1)
          panel.loess(x,y,span=0.5,col=1,lwd=2)})

summary(M6.lme)
anova(M6.lme)

##plotting

grid.newpage()
text <- element_text(size = 18, face="bold") #change the size of the axes
theme_set(theme_bw())

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


ggplot(data = pre, aes(x=time,y=CellsN))+ 
  stat_smooth(method="loess", formula=y~x, color="black", size=2) +
  facet_grid(.~treatment, labeller=mf_labeller)+ 
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


### define coefficients of linear function directly
library(multcomp)
K <- diag(length(coef(M6.lme)))[-1,]
rownames(K) <- names(coef(M6.lme))[-1]
summary(glht(M6.lme,covariate_average=TRUE, interaction_average=TRUE, linfct=K))

summary(glht(M6.lme, covariate_average=TRUE, interaction_average=TRUE))

#combined

pre$trt <- interaction (pre$treatment, pre$time)
model <- lme (CellsN~trt, random = ~1|reptreat,  weights=varIdent(form=~1|reptreat), correlation= corAR1(form=~1|reptreat/treatment), 
               method="REML", data=pre)
comp.try <- glht(model, linfct=mcp(trt="Tukey"), covariate_avergae=TRUE, interaction_average=TRUE)
