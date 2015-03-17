library(gdata)
library(data.table)
library(ggplot2)
library(grid)
library(gtable)


bead2=61.5*10e-4 #in cm
rad=seq(1, 340, 1)*10^-4
D= 10e-5
#method 1 with density

C1=3.52*0.000001 #moles/mg
density= 3.94/1000 #mg/ml

Cstart=(C1*density)*1000 #in M
Cb= Cstart*bead2 #in mol/bead 
Cb2= (Cstart*bead2)*10^9 #in nmol/bead 


i2=Cstart*4*pi*bead2*D
i2 #in mol/s


var3=as.data.frame(cbind(i2,D,rad))

NT2 <- data.table(var3, key="rad")
NT2.2=as.data.frame(NT2[, list(C=(i2/(4*pi*D*sqrt(rad))), tss=rad^2/D), by="rad"]) #mol/cm3
NT2.2$rad2=NT2.2$rad/0.001 # radius from cm to 痠
NT2.2$CM=NT2.2$C*1000 # to M
NT2.2$然=NT2.2$CM*1000000 #M to 然
NT2.2$Clog=log(NT2.2$然)

NT<- subset(NT2.2, NT2.2$rad2>60, )


grid.newpage()
text <- element_text(size = 20, face="bold") #change the size of the axes
theme_set(theme_bw())

ggplot (data=NT, aes(x=rad2, y=然))+geom_line(size=2, color="red")+
  labs(title="Diffusion of dSi or dGe around alox bead", x="Distance from bead (痠)", 
       y="然")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, 
        axis.title.y = element_text(colour = "red"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#method 2 with concentration of solution

Ci=4.44e-6
#computingsteady state conc of Si with respect to distance and time in mol/cm3
i=Ci*4*pi*sqrt(rbead)*D
i #in mol/s

var2=as.data.frame(cbind(i,D,rad))

NT <- data.table(var2, key="rad")
NT1.2=as.data.frame(NT[, list(C=(i/(4*pi*D*(sqrt(rad)))), tss=rad^2/D), by="rad"]) #mol/cm3
NT1.2$rad2=NT1.2$rad/0.0001 # radius from cm to 痠
NT1.2$CM=NT1.2$C*1000 # to M
NT1.2$然=NT1.2$CM*1000000 #M to 然
NT1.2$Clog=log(NT1.2$然)
