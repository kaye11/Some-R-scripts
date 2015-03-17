library(ggplot2)
library(grid)

qplot(X, Y, data=t1 [t1$A<500, ], color = factor(A), group = factor(A))+ geom_point(size=1)+
  geom_path(lineend="round", size=1, arrow=arrow(angle=45, ends="last", type= "closed", length=unit (0.3, "inches")), 
            mapping=aes(group=factor(A))) +theme_classic() + 
  labs(list(title="Control Alox Bead"))+
  theme(axis.text=element_text(size=35, face="bold"), axis.title=element_text(size=35,face="bold"), 
        legend.position="none", plot.title = element_text(size =35, face="bold")) +  
  annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)), y=377 + 53.5*sin(seq(0,2*pi,length.out=100)), size=2)+
  xlim(0, 1000) + ylim (800, 0)