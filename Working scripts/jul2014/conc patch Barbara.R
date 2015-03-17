library(gdata)
library(data.table)
library(ggplot2)
library(grid)
library(gtable)

Cin= 1.4e-9 #in moles
rbead=61.5*0.0001 #in cm
D=1e-5 #in cm2/s
rad=seq(0.0001, 340, 2)*0.0001 #in cm, whole picture ay sakop

#goal mol/cm^3

#bead volume
bead=(4*pi*rbead^3)/3 #volume of the bead in cm^3

Cin2=Cin/bead # in mol/cm^3
Cin3=Cin2*1000 # in M

#computingsteady state conc of Si with respect to distance and time in mol/cm3
i=Cin2*4*pi*rbead*D
i #in mol/s

var2=as.data.frame(cbind(i,D,rad))

NT <- data.table(var2, key="rad")
NT1.2=as.data.frame(NT[, list(C=(i/(4*pi*D*sqrt(rad))), tss=rad^2/D), by="rad"]) #mol/cm3
NT1.2$rad2=NT1.2$rad/0.0001 # radius from cm to µm
NT1.2$CM=NT1.2$C*1000
NT1.2$Clog=log(NT1.2$CM)

write.table(NT1.2, file="d:/Karen's/PhD/R program/Processed_data/steady state.csv", 
            sep=";", col.names=T, row.names=F, )



##plotting two y-axes on ggplot at the same time (actually not possible)
grid.newpage()
text <- element_text(size = 18, face="bold") #change the size of the axes
theme_set(theme_bw())

# scale_y_log10(breaks=c(0.001,.01,.1,1,10))
# two plots 

p1 <- ggplot (data=NT1.2, aes(x=rad2, y=CM))+geom_line(size=2, color="red")+
 labs(title="Steady state concentration of\n dSi or dGe around alox bead", x="Distance from bead (µm)", 
                         y="Log Si concentration (M)")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, 
        axis.title.y = element_text(colour = "red"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <- ggplot (data=NT1.2, aes(x=rad, y=tss))+geom_line(size=2, color="blue")+
  labs(y="Time until steady state (s)")+ theme(axis.text=element_text(size=18), 
                                               axis.title=element_text(size=20,face="bold"), 
                              plot.title = element_text(size =20, face="bold"), axis.text=text, 
                              axis.title.y = element_text(colour = "blue"), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank()) %+replace% theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

ia2 <- which(g2$layout$name == "ylab")
ga2 <- g2$grobs[[ia2]]
ga2$rot <- 90
g <- gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)


# draw it
grid.draw(g)