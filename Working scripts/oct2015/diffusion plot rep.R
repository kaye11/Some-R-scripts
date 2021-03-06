library(gdata)
library(data.table)
library(ggplot2)
library(grid)
library(gtable)
library(plyr)

'rep1' <- read.csv("D:/Karen's/PhD/R program/Processed_data/diffusion/rep1.csv", sep=";")
'rep2' <- read.csv("D:/Karen's/PhD/R program/Processed_data/diffusion/rep2.csv", sep=";")
'rep3' <- read.csv("D:/Karen's/PhD/R program/Processed_data/diffusion/rep3.csv", sep=";")

all <- rbind((rep1 [, c (7, 8, 9)]), (rep2 [, c (7, 8, 9)]), 
             (rep3 [, c (7, 8, 9)]))

dev.new(width=6, height=9)
source("resizewin.R")
resize.win(9,6)

grid.newpage()
text <- element_text(size = 20, face="bold") #change the size of the axes
theme_set(theme_bw())

ggplot (data=all, aes(x=rad2, y=uMsq))+geom_line(size=2)+ 
  labs(x="Distance from bead (�m)", 
       y="�M dSi/bead")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =25, face="bold"), axis.text=text, 
        axis.title.y = element_text(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

source("summarySE.R")
difall<- summarySE(all, measurevar="uMsq", groupvars=c("rad2"), na.rm=TRUE)

ggplot(data=difall, aes(x=rad2, y=uMsq)) + geom_line(size=0.8)+ 
  geom_ribbon(aes(ymin=uMsq-sd, ymax=uMsq+sd), alpha=0.5, size=3) +  
  labs(x="Distance from bead (�m)", 
       y="�M dSi/bead")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = element_text(size=15), strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))+
  geom_vline(xintercept = c(seq(112, 336, 112)), colour="#e6550d", linetype = "longdash", size=2)

difall2=subset(difall, difall$rad2>10, )

ggplot(data=difall2, aes(x=rad2, y=uMsq)) + geom_line(size=0.8)+ 
  geom_ribbon(aes(ymin=uMsq-sd, ymax=uMsq+sd), alpha=0.5, size=3) +  
  labs(x="Distance from bead (�m)", 
       y="�M dSi/bead")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = element_text(size=15), strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))

#for poster

ggplot(data=difall, aes(x=rad2, y=uMsq)) + geom_line(size=2)+ 
   labs(x="Distance from bead (�m)", 
       y="�M dSi/bead")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = element_text(size=15), strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm"))