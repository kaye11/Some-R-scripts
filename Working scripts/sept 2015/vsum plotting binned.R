Vsum<- summarySE(binned, measurevar="V", groupvars=c("cond", "bin", "time2"), na.rm=TRUE)

ggplot(data=Vsum, aes(x=time2, y=V, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=V-se, ymax=V+se), width=30, size=1) + facet_grid(.~bin, labeller=mf_labeller2) + geom_hline(yintercept=0)+
  scale_colour_manual(values = c("lightcoral", "steelblue2"), breaks=c("Control", "Si"),
                      labels=c("Control", "dSi")) +
  labs(list(x = "Time (s)", y = "Sine angle"))+ labs (color="Experimental condition")+
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold"), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(0.5,1,1,1.5), "cm")) + 
  scale_x_continuous (breaks=c(200, 400, 600))