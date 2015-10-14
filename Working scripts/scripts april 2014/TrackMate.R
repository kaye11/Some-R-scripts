##source this! name the file into s1
library (data.table)
library (ggplot2)
library(grid)

##direct input from trackmate 
NT=data.table(s1, key="TRACK_ID")
NT=NT[, list(A=TRACK_ID, X=POSITION_X, Y=POSITION_Y, T=FRAME), by=c("TRACK_ID")]
s1a= NT[order(A, T)]
s1=s1a[,list(T=T, X=X, Y=Y, V=c(0, sqrt(diff(X)^2+diff(Y)^2))), by=c("A")]

##plotting

t1=s1

qplot(X, Y, data = s1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse()+ geom_path(aes(group=factor(A))) 

qplot(X, Y, data=t1, color = factor(A), group = factor(A))+ geom_point(size=1)+
  geom_path(lineend="round", size=1, arrow=arrow(angle=45, ends="last", type= "closed", length=unit (0.20, "inches")), 
            mapping=aes(group=factor(A))) + scale_y_reverse()+theme_classic() + 
  labs(list(title="Control Alox Bead"))+
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
        legend.position="none", plot.title = element_text(size =20, face="bold"))+
  annotate("path",x = 344 + 43*cos(seq(0,2*pi,length.out=100)), y=344 + 43*sin(seq(0,2*pi,length.out=100)), 
           color="black", size=2)  

