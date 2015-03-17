#substrate final model choices

library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)

#Whole dataset, AIC=727.0999
#smoother for time by treatment, smoother for bin by treatment, 
#tensor interaction produces smoothing for main effects (treatment) and lower interactions (time and binn)
#correlation corrected between treatment and replicate (rho=0.3)
#replicate treated as random

WDsub <- gamm(CellsN ~ s(T, by=treatment, bs="cs", k=3) + s (binn, by=treatment, k=3, bs="cs") + 
             ti (T, binn, by=treatment, k=3), method="REML", correlation= corAR1 (form=~1|treatment/reptreat), 
           random=list(reptreat=~1), data=sub)


#BinA AIC= 211.3209
#smoother for time by treatment
#correlation corrected between treatment and replicate (rho=0.3)
#replicate treated as random

BinAsub <- gamm (CellsN~s(T, by=treatment, bs="cs", k=3), method="REML", #change to bs=cs for paper
                 correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), data = BinA) 


#BinB AIC= 246.2429
#smoother for time by treatment
#correlation corrected between treatment and replicate (rho=0.3)
#replicate treated as random

BinBsub <- gamm (CellsN~s(T, by=treatment, bs="cs", k=3), method="REML", 
                 correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), data = BinB) 

#BinC AIC= 250.5875
#smoother for time by treatment
#correlation corrected between treatment and replicate (rho=0.3)
#replicate treated as random

BinCsub <- gamm (CellsN~s(T, by=treatment, bs="cs", k=3), method="REML", 
                 correlation= corAR1 (form=~1|treatment/reptreat), random=list(reptreat=~1), data = BinC) 


##plotting by bin (matrabaho wag gawin)
grid.newpage()
text <- element_text(size = 18) #change the size of the axes
theme_set(theme_bw())


Aplot <- ggplot(data = BinA, aes(x=T,y=CellsN, color=treatment, shape=treatment))+ 
  stat_smooth(method="gam", formula=y~s(x, k=10), size=2, se=TRUE)+ 
  scale_colour_manual(values = c("lightcoral", "seagreen3", "steelblue2")) +
  labs(list(x = "Time (sec)", y = "Normalized cell count", title = "Bin A"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

Bplot <- ggplot(data = BinB, aes(x=T,y=CellsN, color=treatment, shape=treatment))+ 
  stat_smooth(method="gam", formula=y~s(x, k=3), size=2, se=TRUE)+ 
  scale_colour_manual(values = c("lightcoral", "seagreen3", "steelblue2")) +
  labs(list(x = "Time (sec)", title = "Bin B"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

Cplot <- ggplot(data = BinC, aes(x=T,y=CellsN, color=treatment, shape=treatment))+ 
  stat_smooth(method="gam", formula=y~s(x, k=3), size=2, se=TRUE)+ 
  scale_colour_manual(values = c("lightcoral", "seagreen3", "steelblue2")) +
  labs(list(x = "Time (sec)", title = "Bin C"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grid.arrange (Aplot, Bplot, Cplot, ncol=3)

library(gridExtra)
library(plyr)
library(ggplot2)

#plotting

grid.newpage()
text <- element_text(size = 18, face="bold") #change the size of the axes
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

ggplot(data = sub, aes(x=T,y=CellsN, color=treatment))+ 
  stat_smooth(method="gam", formula=y~s(x, k=3), size=2, se=TRUE)+ 
  facet_grid(.~Bin, labeller=mf_labeller)+ scale_colour_manual(values = c("lightcoral", "seagreen3", "steelblue2")) +
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
