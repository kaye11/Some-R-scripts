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
    value[value=="A"] <- "Bin a"
    value[value=="B"]   <- "Bin b"
    value[value=="C"] <- "Bin c"
  }
  return(value)
}

count$treat2 <- factor(count$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))

countplot= ggplot(data = count, aes(x=T,y=CellsN, color=treatment))+ 
  stat_smooth(method="loess", formula=y~x, size=2, se=TRUE)+ 
  facet_grid(Bin~treat2, labeller=mf_labeller)+ scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "Si"),
                                                              labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Normalized cell count"))+ labs (color="Experimental condition")+
    theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
          axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,2), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 

velsum <- summarySE(vel, measurevar="V", groupvars=c("cond","time", "bin"))

mf_labeller2 <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin a"
    value[value=="binB"]   <- "Bin b"
    value[value=="binC"] <- "Bin c"
  }
  return(value)
}

velplot= ggplot(data = vel, aes(x=T,y=V, color=cond))+ 
  stat_smooth(method="gam", formula=y~s(x, k=7), size=2, se=TRUE)+  
  facet_grid(.~bin, labeller=mf_labeller2)+  scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "Si"),
                                                                 labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Mean speed (µm/s)"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,2), "cm")) + 
  scale_x_continuous (breaks=c(200, 400, 600)) 

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


#distance plot (distorder2)
distorder2$distmm=distorder2$sum/1000

distsum<- summarySE(binned2, measurevar="dist", groupvars=c("cond", "bin", "T"), na.rm=TRUE)

distorder2$cond2 <- factor(distorder2$cond, levels=c("Con", "Si"), labels=c("Control", "dSi"))


distplot=ggplot(data=distorder2, aes(x=time, y=distmm, color=cond2)) +  stat_smooth(aes(group=cond), method="loess", size=2, se=TRUE)+
  labs(list(x = "Time (s)", y = "Sum distance \n from the bead (mm)")) +
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Con", "Si"), labels=c("Control", "dSi")) + 
  facet_grid(bin~cond2, label=mf_labeller2, scales="free_y")+
  labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=0.50), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + 
  scale_x_discrete (breaks=c("0", "200", "400", "600"))


grid.arrange(countplot, velplot, distplot, ncol=1)


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
