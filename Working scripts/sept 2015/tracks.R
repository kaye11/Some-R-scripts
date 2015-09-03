
binned <- read.csv("d:/Karen's/PhD/R program/Processed_data/binnedforgamm.csv", header=TRUE, sep=";")

library(ggplot2)
library(grid)
library(ggthemes)
library(gridExtra)
library(reshape2)


qplot(X, Y, data = binned [binned$cond=="Con", ], color = factor(ID), group = factor(ID))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse()

grid.newpage()
text <- element_text(size = 20, face="bold") #change the size of the axes
theme_set(theme_bw()) 

qplot(X, Y, data = Si [Si$A== "0", ]) +
  scale_colour_gradient(low="lightgrey", high="black") + geom_point(aes(color=V, size=V))
 labs(list(X="X", Y = "Y", color="Velocity (µm/s)")) + scale_y_reverse()


library(data.table)
data.table (Si, key = "A")

qplot(X, Y, data = pherdatacomp [pherdatacomp$AR== "229-DPR018", ]) +
  scale_colour_gradient(low="lightgrey", high="black") + 
  geom_path(aes(color=V), size=2, lineend="square", arrow=arrow(angle=45, ends="last", type= "closed", length=unit (0.1, "inches"))) + 
  annotate("path",x = 277 + 17*cos(seq(0,2*pi,length.out=100)), y=281 + 17*sin(seq(0,2*pi,length.out=100)), size=2)+
  xlim(150, 420) + ylim (200, 600)+
  labs(list(X="X", Y = "Y", color="Velocity (µm/s)")) + scale_y_reverse() +
  theme(axis.text=element_text(size=25), axis.title=element_text(size=35,face="bold"), 
        plot.title = element_text(size =35, face="bold"),
        legend.key.width=unit(1,"cm"),legend.key.height=unit(1,"cm"), legend.position=c(0.75, 0.92), 
        legend.direction="horizontal", 
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=element_text(size=15), 
        panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

##geom_path does not work because time data is not continous (binning problem)
