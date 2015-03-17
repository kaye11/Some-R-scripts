##plotting two y-axes on ggplot at the same time (actually not possible)
grid.newpage()
text <- element_text(face = "bold", size = 15) #change the size of the axes
theme_set(theme_bw())


# two plots
p1 <- ggplot (data=NT1.2, aes(x=rad, y=C))+geom_line(size=1, color="red")+geom_point(size=2.5, color="red")+ 
  scale_y_log10(breaks=c(0.001,.01,.1,1,10))+  labs(title="Steady state concentration of Si from alox bead", x="Distance from bead (µm)", 
                                                    y="Log Si concentration (µm)")+
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text, 
        axis.title.y = element_text(colour = "red"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <- ggplot (data=NT1.2, aes(x=rad, y=tss))+geom_line(size=1, color="blue")+geom_point(size=2.5, color="blue")+  
  labs(y="Time until steady state (sec)")+ theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
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