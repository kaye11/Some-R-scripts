library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)



count$T=count$time*60

countsum <- summarySE(count, measurevar="CellsN", groupvars=c("treatment","T", "Bin"))

grid.newpage()
text <- element_text(size = 20) #change the size of the axes
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

countplot= ggplot(data = count, aes(x=T,y=CellsN, color=treatment))+ 
  stat_smooth(method="loess", formula=y~x, size=2, se=TRUE)+ 
  facet_grid(.~Bin, labeller=mf_labeller)+ scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "Si"),
                                                              labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
    theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
          axis.title.x=element_text(size=20,face="bold", vjust=-0.02),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))

velsum <- summarySE(vel, measurevar="V", groupvars=c("cond","time", "bin"))

mf_labeller2 <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
  }
  return(value)
}

velplot= ggplot(data = vel, aes(x=T,y=V, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7), size=2, se=TRUE)+  
  facet_grid(.~bin, labeller=mf_labeller2)+  scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "Si"),
                                                                 labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Mean speed (µm/s)"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.02),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))

grid.arrange (countplot, velplot, ncol=1)

grid.arrange(countplot, velplot, accplot, ncol=1)


accsum <- summarySE(vel, measurevar="Ac", groupvars=c("cond","time", "bin"), na.rm=TRUE)

accplot= ggplot(data = vel, aes(x=T,y=Ac, color=cond))+ geom_smooth(method="loess", size=2)+ 
  facet_grid(.~bin, labeller=mf_labeller2)+ 
  labs(x = "Time (sec)")+ labs (color="Experimental Condition")+ ylab(expression(Acceleration (µm^2/s)))+
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text,  axis.text.y=text, legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#for ppt


countplot2= ggplot(data = count, aes(x=T,y=CellsN, color=treatment))+ 
  stat_smooth(method="loess", formula=y~x, size=2, se=TRUE)+ 
  facet_grid(.~Bin, labeller=mf_labeller)+ scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "Si"),
                                                               labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.02),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))

velplot2= ggplot(data = vel, aes(x=T,y=V, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7), size=2, se=TRUE)+  
  facet_grid(.~bin, labeller=mf_labeller2)+  scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "Si"),
                                                                 labels=c("Control", expression (Si(OH)["4"]))) +
  labs(list(x = "Time (s)", y = "Mean speed (µm/s)"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.02),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=element_blank(), legend.text=text, panel.margin=unit (1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))

grid.arrange (countplot2, velplot2, ncol=1)
