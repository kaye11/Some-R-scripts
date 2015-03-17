library(ggplot2)
library(grid)

qplot(X, Y, data=t1 [t1$A<1500, ], color = factor(A), group = factor(A))+ geom_point(size=1)+
  geom_path(lineend="round", size=1, arrow=arrow(angle=30, ends="last", type= "closed", length=unit (0.3, "inches")), 
            mapping=aes(group=factor(A))) +theme_classic() + 
  labs(list(title="Silicon Alox Bead"))+
  theme(axis.text=element_text(size=35, face="bold"), axis.title=element_text(size=35,face="bold"), 
        legend.position="none", plot.title = element_text(size =35, face="bold")) +  
  annotate("path", x = 452+61.5*cos(seq(0,2*pi,length.out=100)), y=345+61.5*sin(seq(0,2*pi,length.out=100)), size=2)+
  xlim(0, 1000) + ylim (800, 0)