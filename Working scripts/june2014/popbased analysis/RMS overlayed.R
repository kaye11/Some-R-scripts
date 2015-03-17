library(ggplot2)
library(grid)
library(ggthemes)
library(gridExtra)
library(reshape2)

grid.newpage()
text <- element_text(size = 15) #change the size of the axes
theme_set(theme_bw())

##RMS plotting overlayed
df <- data.frame(RMS.Si, RMS.control)
df$Si=df$MF
df$control=df$MF.1


ggplot(df, aes(time, y = value, color = Treatment)) + 
  geom_point(aes(y = Si, col = "Si"), size=3) + 
  geom_point(aes(y = control, col = "Control"), size=3)+ labs(list(x = "Time (s)", y = "RMS (µm)")) + 
  scale_colour_manual(values = c("black","red")) +
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text,  
        legend.position=c(0,1), legend.justification=c(0,1),
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

countplot= ggplot(data = count, aes(x=T,y=CellsN, color=treatment))+ 
  stat_smooth(method="gam", formula=y~s(x, k=10), size=2)+ 
  facet_grid(.~Bin, labeller=mf_labeller)+ 
  labs(list(x = "Time (sec)", y = "Normalized Cell Count"))+ labs (color="Experimental Condition")+
  