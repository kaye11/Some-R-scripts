## Test is i can plot distance from the bead  and swimming ngle of the cells

con.binned$distancetobead = sqrt((455-con.binned$X)^2 + (377-con.binned$Y)^2)

con.binned2 = con.binned[complete.cases(con.binned),]
controldata <- ddply(con.binned, .(X,Y), summarize, meanangs = mean(angs, na.rm = TRUE))

si.binned$distancetobead = sqrt((452-si.binned$X)^2 + (345-si.binned$Y)^2)

si.binned2 = si.binned[complete.cases(si.binned),]
sidata <- ddply(si.binned, .(X,Y), summarize, meanangs = mean(angs, na.rm = TRUE))

c = qplot(x=T, y=distancetobead, data = si.binned, geom = c("smooth"), color = A, group = A) + 
  labs("Si") + geom_hline(yintercept = 61.5,colour = "red") + 
  scale_y_continuous(limits =c(0,620))+ scale_x_continuous(limits =c(0,620))+
  theme(legend.position="none") +labs(title="Si", x="Time", y="Distance to bead")+ 
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size =20, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))

d = qplot(T,distancetobead, data = con.binned, geom = c( "smooth"),color = A, group = A) + 
  labs("Control") + geom_hline(yintercept = 53.5,colour = "red") + 
  scale_y_continuous(limits =c(0,620))+ scale_x_continuous(limits =c(0,620))+
  theme(legend.position="none") +labs(title="Control", x="Time", y="Distance to bead")+ 
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size =20, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))

grid.arrange(d,c, ncol=2)



#angs over time. 
qplot(T,angs, data = si.binned, geom = c( "point","smooth"))
qplot(T,angs, data = con.binned, geom = c( "point","smooth"))


a = qplot(distancetobead,angs, data = con.binned, geom = c( "point","smooth"))
b = qplot(distancetobead,angs, data = si.binned, geom = c( "point","smooth"))
grid.arrange(a, b, ncol=1)
