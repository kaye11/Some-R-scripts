
my.shape <- draw.circle (455,351,seq(112,336, 112))
my.points <- data.frame(x=t1$X, y=t1$Y)
my.points$in.shape <- 1:25116 %in% inpip(my.points, my.shape)

plot(my.points[1:2], col=1 + my.points$in.shape)
polygon(my.shape)

library(plotrix)
plot(0,0,type = "n", xlim = c(0,957), ylim = c(0,765))
my.shape=draw.circle (455,351,seq(112,336, 112))
my.points=df
my.points$in.shape <- 1:500 %in% inpip(my.points, my.shape(c[x,y]))
plot(my.points[1:2], col=1 + my.points$in.shape)

seq(0,4,.5))