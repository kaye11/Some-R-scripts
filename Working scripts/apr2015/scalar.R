library(data.table)
library(ggplot2)

Con$cond="Con"
Si$cond="Si"

# Combine A and cond
Con$ID <- paste(Con$A, Con$cond, sep = "-")
Si$ID <- paste(Si$A, Si$cond, sep = "-")

Con$alox.X=455
Con$alox.Y=377
Si$alox.X=452
Si$alox.Y=345

#subsetting
ConBinA=Con[Con$bin=="binA", ]
ConBinB=Con[Con$bin=="binB", ]
ConBinC=Con[Con$bin=="binC", ]

SiBinA=Con[Si$bin=="binA", ]
SiBinB=Con[Si$bin=="binB", ]
SiBinC=Con[Si$bin=="binC", ]

source("summarySE.R")
SiSum <- summarySE(Si, measurevar="NGDR", groupvars=c("bin"), na.rm=TRUE)
ConSum <- summarySE(Con, measurevar="NGDR", groupvars=c("bin"), na.rm=TRUE)

library(plyr)

binned=rbind(Con, Si)

binned$scalar2=sqrt(binned$scalar)

binned=binned[binned$bin != "outbin", ] 

Sumall <- ddply(binned, c("T", "bin", "cond"), summarise,
               N    = length(scalar2),
               mean = mean(scalar2, na.rm=TRUE),
               sum= sum(scalar2, na.rm=TRUE), 
               sd   = sd(scalar2, na.rm=TRUE),
               se   = sd / sqrt(N), na.rm=TRUE)

Sumall2 <- ddply(binned, c("bin", "cond"), summarise,
                N    = length(scalar),
                mean = mean(scalar,  na.rm=TRUE),
                sd   = sd(scalar),
                se   = sd / sqrt(N))

grid.newpage()
text <- element_text(size = 30, face="bold") #change the size of the axes
theme_set(theme_bw())


mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin a"
    value[value=="binB"]   <- "Bin b"
    value[value=="binC"] <- "Bin c"
  }
  return(value)
}

ggplot(data=Sumbin, aes(x=T, y=sum, color=cond)) +  geom_line() + geom_point()+
  labs(list(x = "Experimental condition", y = "Distance from the bead (µm)")) +
  facet_grid(~cond) +
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Control", "Si"), labels=c("Control", "dSi")) +
  theme(axis.text.y=element_text(size=30), axis.text.x=element_text(size=30, face="bold"),
        axis.title=element_text(size=35,face="bold"),  axis.title.x = element_blank(),
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text,  
        legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +geom_hline(yintercept=0)


#distance
binned$dist=sqrt(((binned$alox.X-binned$X)^2)+((binned$alox.Y-binned$Y)^2))

distall <- ddply(binned, c("T", "bin", "cond"), summarise,
                N    = length(dist),
                mean = mean(dist, na.rm=TRUE),
                sum= sum(dist, na.rm=TRUE), 
                sd   = sd(dist, na.rm=TRUE),
                se   = sd / sqrt(N), na.rm=TRUE)


ggplot(data=distall, aes(x=T, y=sum, color=cond)) +  geom_line() + geom_point()+
  labs(list(x = "Experimental condition", y = "Sum distance from the bead (µm)")) +
  facet_grid(~cond) +
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Control", "Si"), labels=c("Control", "dSi")) +
  theme(axis.text.y=element_text(size=30), axis.text.x=element_text(size=30, face="bold"),
        axis.title=element_text(size=35,face="bold"),  axis.title.x = element_blank(),
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text,  
        legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggplot(data=distall,  aes(x=T, y=mean, color=cond)) +  geom_line() + geom_point()+
  labs(list(x = "Experimental condition", y = "Mean distance from the bead (µm)")) +
  facet_grid(~cond) +
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Control", "Si"), labels=c("Control", "dSi")) +
  theme(axis.text.y=element_text(size=30), axis.text.x=element_text(size=30, face="bold"),
        axis.title=element_text(size=35,face="bold"),  axis.title.x = element_blank(),
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text,  
        legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggplot(data=distall, aes(x=T, y=N, color=cond)) +  geom_line() + geom_point()+
  labs(list(x = "Experimental condition", y = "Number of cells")) +
  facet_grid(~cond) +
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Control", "Si"), labels=c("Control", "dSi")) +
  theme(axis.text.y=element_text(size=30), axis.text.x=element_text(size=30, face="bold"),
        axis.title=element_text(size=35,face="bold"),  axis.title.x = element_blank(),
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text,  
        legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


tm=seq(0, 600, by = 30)
binned$time <- cut(binned$T, tm, labels=paste(tail(tm, -1L)))

binned$time[is.na(binned$time)] <- 30 #replace NAs with 30. some data time points have the starting point as NA

binned$time=as.factor(binned$time)

distall_time <- ddply(binned, c("time", "bin", "cond"), summarise,
                 N    = length(dist),
                 mean = mean(dist),
                 sum = sum (dist), 
                 sd   = sd(dist),
                 se   = sd / sqrt(N))

distall_time2 <- ddply(binned, c("time", "cond"), summarise,
                      N    = length(dist),
                      mean = mean(dist, na.rm=TRUE),
                      sum= sum(dist, na.rm=TRUE), 
                      sd   = sd(dist, na.rm=TRUE),
                      se   = sd / sqrt(N), na.rm=TRUE)

#distall_time$time <- as.numeric(as.character(distall_time$time))

ggplot(data=distall_time, aes(x=time, y=sum, color=cond)) +  geom_boxplot() +
  labs(list(x = "Experimental condition", y = "Sum distance from the bead (µm)")) + 
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Control", "Si"), labels=c("Control", "dSi")) + facet_grid(~cond)+
  theme(axis.text.y=element_text(size=30), axis.title.y=element_text(size=40,face="bold", vjust=-0.05), 
        plot.title = element_text(size =40, face="bold"), axis.title.x = element_blank(), 
        axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,2), "cm"), panel.border = element_blank(),
        panel.background = element_blank(), panel.margin=unit(2, "lines"), axis.line = element_line(colour = "black")) +
  scale_x_discrete (breaks=c("30", "300", "600"))

ggplot(data=distall_time, aes(x=time, y=sum, color=cond)) +  geom_point () + geom_line(aes(group=cond))+ 
  labs(list(x = "Experimental condition", y = "Sum distance from the bead (µm)")) + 
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Control", "Si"), labels=c("Control", "dSi")) + facet_grid(bin~cond, label=mf_labeller, scales="free_y")+
  theme(axis.text.y=element_text(size=30), axis.title.y=element_text(size=40,face="bold", vjust=-0.05), 
        plot.title = element_text(size =40, face="bold"), axis.title.x = element_blank(), 
        axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,2), "cm"), panel.border = element_blank(),
        panel.background = element_blank(), panel.margin=unit(2, "lines"), axis.line = element_line(colour = "black")) +
  scale_x_discrete (breaks=c("30", "300", "600"))



ggplot(data=distall_time, aes(x=time, y=sum, color=cond)) +  stat_smooth(aes(group=cond), method="loess", size=2, se=TRUE)+
  labs(list(x = "Experimental condition", y = "Sum distance from the bead (µm)")) + 
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Con", "Si"), labels=c("Control", "dSi")) + facet_grid(bin~., label=mf_labeller, scales="free_y")+
  labs (color="Experimental condition")+
  theme(axis.text.y=element_text(size=30), axis.title.y=element_text(size=40,face="bold", vjust=-0.05), 
        plot.title = element_text(size =40, face="bold"), axis.title.x = element_blank(), 
        axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,2), "cm"), panel.border = element_blank(),
        panel.background = element_blank(), panel.margin=unit(1, "lines"), axis.line = element_line(colour = "black")) +
  scale_x_discrete (breaks=c("30", "300", "600"))

#ordering and normalizing the start
distorder <- arrange(distall_time, cond, time) 
Sidist=distorder[61:119,]
Condist=distorder[1:60,]

write.table (distorder, "d:/Karen's/PhD/R program/distorder.csv", 
             sep=";", col.names=T, row.names=F)


grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw()) 


ggplot(data=distorder2, aes(x=time, y=relsum, color=cond)) +  stat_smooth(aes(group=cond), method="loess", size=2, se=TRUE)+
  labs(list(x = "Time (s)", y = "Relative sum distance from the bead (µm)")) + 
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Con", "Si"), labels=c("Control", "dSi")) + facet_grid(.~bin, label=mf_labeller, scales="free_y")+
  labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.02),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_x_discrete (breaks=c("0", "200", "400", "600"))
