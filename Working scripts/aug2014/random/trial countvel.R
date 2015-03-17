library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)

vel <- droplevels( all.binned[-which(all.binned$bin == "outbin"), ] )
count$T=count$time*60

grid.newpage()
text <- element_text(face = "bold", size = 15) #change the size of the axes
theme_set(theme_bw())

countsum <- summarySE(count, measurevar="CellsN", groupvars=c("Bin","T", "treatment"))

ggplot(countsum [countsum$Bin=="A", ], aes(x=T, y=CellsN, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=CellsN-se, ymax=CellsN+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(1.5))
# Error bars represent standard error of the mean
ggplot(dfc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-se, ymax=len+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

p1=ggplot(data=vel [vel$bin=="binA", ], aes (x=T, y=V, group=cond, color=cond)) + geom_smooth(method="loess", size=1) + 
  labs(title="Mean Velocity (Binned)", x="Time", y="Mean Velocity (µm/sec)")+ 
  theme(axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =25, face="bold"), 
        legend.title = element_text(colour="black", size=15, face="bold"), 
        legend.text = element_text(colour="black", size = 15, face = "bold"), 
        axis.text=text, strip.text.x = element_text(size = 15, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ 
  scale_colour_discrete(name  ="Treatment", labels=c("Control", "Silica"))

p2= ggplot(data = count [count$Bin=="A", ], aes(x=T,y=CellsN, colour = treatment, group=treatment))+ 
  geom_bar(method="loess", size=2)+ 
  labs(list(title="Exponential Phase", x = "Time", y = "Normalized Cell Count")) + 
  theme(axis.text=element_text(size=20, face="bold"), axis.title=element_text(size=20,face="bold"), 
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
