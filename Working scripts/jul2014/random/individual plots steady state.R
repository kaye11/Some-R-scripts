
text <- element_text(face = "bold", size = 15) #change the size of the axes
theme_set(theme_bw())

ggplot (data=NT1.2, aes(x=rad, y=C))+geom_line(size=1)+geom_point(size=2.5)+ scale_y_log10()+ 
  labs(title="Steady state concentration", x="Distance from bead (µm)", y="Log Si concentration (µm)")+
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  annotate("rect", xmin = 0, xmax = 112, ymin=0, ymax=10, alpha = .2)+ 
  annotate("rect", xmin = 112, xmax = 224, ymin=0, ymax=10, alpha = .2, fill="#009933")+
  annotate("rect", xmin = 224, xmax = 336, ymin=0, ymax=10, alpha = .2, fill="#CC6600")+
  annotate("rect", xmin = 336, xmax = 500, ymin=0, ymax=10, alpha = .2, fill="#003399")

ggplot (data=NT1.2, aes(x=rad, y=C))+geom_line(size=1)+geom_point(size=2.5)+  
  labs(title="Steady state concentration", x="Distance from bead (µm)", y="Si concentration (µm)")+
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot (data=NT1.2, aes(x=rad, y=tss))+geom_line(size=1)+geom_point(size=2.5)+  
  labs(title="Time for steady state concentration", x="Distance from bead (µm)", y="Time (s)")+
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 