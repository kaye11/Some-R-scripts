library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)


#read data
si <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/ VM4-1-si-007(2) .csv", sep=";")
con <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/ VM4-1-control-008 .csv", sep=";")

si$cond="dSi"
con$cond="Control"

vm4 <- rbind(con, si)

#plotting
grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw()) 

mf_labeller2 <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
  }
  return(value)
}

ggplot(data = vm4, aes(x=T,y=V, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7), size=2, se=TRUE)+  
  facet_grid(.~bin, labeller=mf_labeller2)+  scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "dSi"),
                                                                 labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Mean speed (µm/s)"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(0.5,1,1,1.5), "cm")) + 
  scale_x_continuous (breaks=c(200, 400, 600)) 


##old data
si_old <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/day3 si.binned.cor .csv", sep=";")
con_old <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/day3 control.binned.cor .csv", sep=";")

si_old$scalar=NULL
si_old$angle=NULL
si_old$cond="dSi"

con_old$scalar=NULL
con_old$angle=NULL
con_old$cond="Control"


#combine two datasets
combineddata<- rbind(con_old, con, si_old, si)

#drop outbin
combineddata <- combineddata[combineddata$bin!="outbin", ]

ggplot(data = combineddata, aes(x=T,y=V, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7), size=2, se=TRUE)+  
  facet_grid(.~bin, labeller=mf_labeller2)+  scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "dSi"),
                                                                 labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Mean speed (µm/s)"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(0.5,1,1,1.5), "cm")) + 
  scale_x_continuous (breaks=c(200, 400, 600)) 
