## Test is i can plot distance from the bead  and swimming ngle of the cells

controldataraw$distancetobead = sqrt((455-controldataraw$X)^2 + (377-controldataraw$Y)^2)

#controldataraw2 = controldataraw[complete.cases(controldataraw),]
controldata <- ddply(controldataraw, .(X,Y), summarize, meanangs = mean(angs, na.rm = TRUE))

sidataraw$distancetobead = sqrt((452-sidataraw$X)^2 + (345-sidataraw$Y)^2)
sidataraw$si.x=452-sidataraw$X
sidataraw$si.y=345-sidataraw$Y


qplot(T, angs, data=sidataraw [sidataraw$A=="2553", ], geom="smooth")

ggplot (data=sidataraw [sidataraw$A=="2553", ], aes (T, y=value))+ geom_point (aes (y=si.x, col="X")) +
          geom_point (aes (y=si.y, col="Y"))+ geom_point (aes (y=distancetobead, col="dist")) +
  geom_point (aes (y=NGDR, col="NGDR"))
  
ggplot (data=sidataraw [sidataraw$A=="3026", ], aes (y=NGDR, x=T))+ 
  geom_point()

ggplot (data=controldataraw [controldataraw$A=="1298", ], aes (y=NGDR, x=T))+ 
  geom_point()



#sidataraw2 = sidataraw[complete.cases(sidataraw),]
sidata <- ddply(sidataraw, .(X,Y), summarize, meanangs = mean(angs, na.rm = TRUE))

c = qplot(T,distancetobead, data = sidataraw, geom = c( "smooth"), color = A, group = A) + 
  labs("Si") + geom_hline(yintercept = 61.5,colour = "red") + 
  scale_y_continuous(limits =c(0,620))+ scale_x_continuous(limits =c(0,620))

d = qplot(T,distancetobead, data = controldataraw, geom = c( "smooth"),color = A, group = A) + 
  labs("Control") + geom_hline(yintercept = 53.5,colour = "red") + 
  scale_y_continuous(limits =c(0,620))+ scale_x_continuous(limits =c(0,620))

grid.arrange(d,c, ncol=2)


#angs over time. 
qplot(T,angs, data = sidataraw, geom = c( "point","smooth"))
qplot(T,angs, data = controldataraw, geom = c( "point","smooth"))


a = qplot(distancetobead,angs, data = controldataraw, geom = c( "point","smooth"))
b = qplot(distancetobead,angs, data = sidataraw, geom = c( "point","smooth"))
grid.arrange(a, b, ncol=1)

si.x=ddply (si.binned, .(X. summarize, ))