library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

write.table (pre, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/prelim.csv", 
             sep=";", col.names=T, row.names=F)


##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
source ("AED.R")

exp=as.data.frame(data.table(cbind(treatment=pre$treatment, T=pre$time, A=pre$A, ID=pre$reptreat)))
cor(exp, method = "spearman")
cor.test(exp$bin, exp$treatment, method="spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)

boxplot(CellsN~treatment, data=pre)
boxplot(CellsN~reptreat, data=pre)
boxplot (CellsN~time, data=pre)

#gam
A <- gam (CellsN~s(time, by=treatment), method="REML", data = pre)
A1 <- gam (CellsN~s(time, by=treatment, bs="cr"), method="REML", data = pre)
A2 <- gam (CellsN~s(time, by=treatment, bs="cs"), method="REML", data = pre) 
A3 <- gam (CellsN~te(time, by=treatment), method="REML", data = pre) 
A4 <- gam (CellsN~s(time, by=treatment, bs="cs", k=5) + s(reptreat, bs="re"), method="REML", data = pre) #best

AIC (A, A1, A2, A3, A4)

#gamm
BA <- gamm (CellsN~s(time, by=treatment), method="REML", data = pre)
BA1 <- gamm (CellsN~s(time, by=treatment, bs="cr"), method="REML", data = pre)
BA2 <- gamm (CellsN~s(time, by=treatment, bs="cs", k=5), method="REML", data = pre)
BA3 <- gamm (CellsN~te(time, by=treatment), method="REML", data = pre) 


f1 <- CellsN~s(time, by=treatment, bs="cs", k=5)

BA4 <- gamm (f1, method="REML",  random=list(reptreat=~1), data = pre) 
BA5 <- gamm (f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), data = pre) #best
#BA6 <- gamm (f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), weights = varIdent(form=~1| reptreat), 
             data = pre) #best
#BA7 <- gamm (f1, method="REML", correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), 
             data = pre)

anova(BA$lme, BA1$lme, BA2$lme, BA3$lme, BA4$lme, BA5$lme)

##plotting
grid.newpage()
text <- element_text(size = 18, face="bold") #change the size of the axes
theme_set(theme_bw())

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="treatment") { 
    value[value=="control"] <- "Control"
    value[value=="t1"]   <- "T1 (0.088 nmoles/bead)"
    value[value=="t2"] <- "T2 (1.4 nmoles/bead)"
    value[value=="t3"] <- "T3 (2.23 nmoles/bead)"
  }
  return(value)
}


ggplot(data = pre, aes(x=time,y=CellsN, shape=treatment))+ geom_point(size=2) +
  stat_smooth(method="gam", formula=y~s(x, k=5), size=1.5, color="black") +
  facet_grid(.~treatment, labeller=mf_labeller)+ 
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())