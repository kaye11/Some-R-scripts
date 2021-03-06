library(gdata)
library(data.table)
library(ggplot2)
library(grid)
library(gtable)

bead2=61.5*0.0001 #in cm
rad=seq(1, 340, 1)*0.0001
D= 10^-5
Ci=7.81232E-06*0.01 #in mol/cm3
i=Ci*4*pi*bead2*D
i #in mol/s

var2=as.data.frame(rad)

var2$mcm=i/(4*pi*D*var2$rad)
var2$mcmsq=i/(4*pi*D*sqrt(var2$rad))
var2$M=var2$mcm*10^3
var2$Msq=var2$mcmsq*10^3
var2$uM=var2$M*10^6
var2$uMsq=var2$Msq*10^6
var2$rad2=var2$rad/0.0001
var2$ts=var2$rad^2/D
var2$ts2=var2$rad2^2/1000
var2$nMsq=var2$Msq*10^9

var=subset(var2, var2$rad2>20, )

grid.newpage()
text <- element_text(size = 20, face="bold") #change the size of the axes
theme_set(theme_bw())

ggplot (data=var2, aes(x=rad2, y=uMsq))+geom_line(size=2, color="red")+
  labs(x="Distance from bead (�m)", 
       y="�M")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =25, face="bold"), axis.text=text, 
        axis.title.y = element_text(colour = "red"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(...)+...+labs(x=expression(y[2]))