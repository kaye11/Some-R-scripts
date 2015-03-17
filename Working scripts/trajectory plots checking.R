library (ggplot2)
library(grid)

qplot(X, Y, data = t1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse()+ geom_path(aes(group=factor(A))) 

qplot(X, Y, data=t1 [t1$A>0 & t1$A<21, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>20 & t1$A<41, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>40 & t1$A<61, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>60 & t1$A<81, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>80 & t1$A<101, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>100 & t1$A<121, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>120 & t1$A<141, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>140 & t1$A<161, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>160 & t1$A<181, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>180 & t1$A<201, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>200 & t1$A<221, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>220 & t1$A<241, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>240 & t1$A<261, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>260 & t1$A<281, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>280 & t1$A<301, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>300 & t1$A<321, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>320 & t1$A<341, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>340 & t1$A<361, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>360 & t1$A<381, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>380 & t1$A<401, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()



qplot(X, Y, data=t1 [t1$A>400 & t1$A<451, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>450 & t1$A<501, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>500 & t1$A<551, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>550 & t1$A<601, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>600 & t1$A<651, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>650 & t1$A<701, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>700 & t1$A<751, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>750 & t1$A<801, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>750 & t1$A<801, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>800 & t1$A<851, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse() +
  geom_path(arrows = arrow(angle = 15, ends = "both", length = unit(0.6, "inches")))

##for ends, kelangan ng grid

qplot(X, Y, data=t1 [t1$A>800 & t1$A<851, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + 
  geom_path(lineend="round", arrow=arrow(angle=45, ends="last", type= "closed", length=unit (0.1, "inches")), 
            mapping=aes(group=factor(A))) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A<500, ], color = factor(A), group = factor(A))+ geom_point(size=1)+
  geom_path(lineend="round", size=1, arrow=arrow(angle=45, ends="last", type= "closed", length=unit (0.3, "inches")), 
            mapping=aes(group=factor(A))) +theme_classic() + 
  labs(list(title="Control Alox Bead"))+
  theme(axis.text=element_text(size=35, face="bold"), axis.title=element_text(size=35,face="bold"), 
        legend.position="none", plot.title = element_text(size =35, face="bold")) +  
  annotate("path",x = 455 + 53.5*cos(seq(0,2*pi,length.out=100)), y=377 + 53.5*sin(seq(0,2*pi,length.out=100)), size=2)+
  xlim(0, 1000) + ylim (800, 0)