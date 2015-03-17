library(ggplot2)
library(data.table)
library(ggthemes)
library(gridExtra)
library(grid)

#previous work

Cin= 1.4 #in nmoles
Cin2 = 1.35e-14
rbead=sqrt(61.5) #in µm
D=1000 #in µm2/s
rad=seq(1, 340, 1) #in µm, whole picture ay sakop

i=Cin*4*pi*rbead*D #nmoles cm3/s

var2=as.data.frame(cbind(i,D,rad))

NT <- data.table(var2, key="rad")
NT1.2=as.data.frame(NT[, list(C=(i/(4*pi*D*(sqrt(rad)))), tss=rad^2/D), by="rad"]) #nmol/cm3
NT1.2$pervol=(NT1.2$C/1000)/0.003 # µm/l


grid.newpage()
text <- element_text(size = 20, face="bold") #change the size of the axes
theme_set(theme_bw())

ggplot (data=NT1.2, aes(x=rad, y=C))+geom_line(size=2, color="red")+
  labs(title="Diffusion of dSi or dGe around alox bead", x="Distance from bead (µm)", 
       y="µmoles released")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, 
        axis.title.y = element_text(colour = "red"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot (data=NT1.2, aes(x=rad, y=pervol))+geom_line(size=2, color="red")+
  labs(title="Diffusion of Si (dis) or dGe around alox bead", x="Distance from bead (µm)", 
       y="µM dSi")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =25, face="bold"), axis.text=text, 
        axis.title.y = element_text(colour = "red"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


write.table(NT1.2, file="d:/Karen's/PhD/R program/Processed_data/diffusion.csv", 
            sep=";", col.names=T, row.names=F, )

ggplot (data=NT1.2, aes(x=rad, y=tss))+geom_line(size=2, color="blue")+
  labs(title="Diffusion of dSi or dGe around alox bead", x="Distance from bead (µm)", 
       y="Time (s)")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, 
        axis.title.y = element_text(colour = "blue"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())