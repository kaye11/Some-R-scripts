
x <- 0:10
y <- smooth$C
lo <- loess(y~x)
plot1=plot(x,y, xlab="Time (min)", ylab="% Density", pch=16, cex=1.5, 
           xlim=c(0,10), ylim=c(20, 60) )
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
lines(xl, predict(lo,xl), col='red', lwd=3)
par(new=T)
x <- 0:10
y <- smooth$T1
lo <- loess(y~x)
plot2=plot(x,y, xlab="Time (min)", ylab="% Density", pch=15, cex=1.5,
           xlim=c(0,10), ylim=c(20, 60) )
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
lines(xl, predict(lo,xl), col='dark green', lwd=3)
par(new=T)
y <- smooth$T2
lo <- loess(y~x)
plot2=plot(x,y, xlab="Time (min)", ylab="% Density", pch=17, cex=1.5,
           xlim=c(0,10), ylim=c(20, 60) )
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
lines(xl, predict(lo,xl), col='blue', lwd=3)
par(new=T)
y <- smooth$T3
lo <- loess(y~x)
plot2=plot(x,y, xlab="Time (min)", ylab="% Density", pch=18, cex=1.5,
           xlim=c(0,10), ylim=c(20, 60) )
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
lines(xl, predict(lo,xl), col='dark violet', lwd=3)



legend(5,30,c("Control","T1", "T2", "T3"), pch = c(15, 16, 17, 18))
legend(7,30, c("Control Smooth","T1 Smooth", "T2 Smooth", "T3 Smooth"),lty=1, lwd=3, 
       col=c("red", " dark green", "blue", "dark violet"))

