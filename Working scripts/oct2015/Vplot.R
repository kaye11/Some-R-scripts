


speedsumall <- ddply(old_data, c("cond", "bin", "time"), summarise,
                 N    = length(V),
                 ID   = length(unique(ID)),
                 mean = mean(V, na.rm=TRUE),
                 sd   = sd(V, na.rm=TRUE),
                 se   = sd / sqrt(N))


speedsumall2 <- subset(speedsumall, speedsumall$ID>1 & speedsumall$time>0,  )

mf_labeller2 <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
  }
  return(value)
}

grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw()) 


source("resizewin.R")
resize.win(9,6)


ggplot(data=speedsumall2, aes(x=time, y=mean, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=20, size=1) + facet_grid(~bin, labeller=mf_labeller2)+
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Mean cell speed (µm/s)"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=1.5), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.5),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 