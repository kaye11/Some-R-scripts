library(data.table)
deg<-180/pi
##What is the xy position of the bead in this video
angStr <- readline("Enter xy position of bead: ");
a <- as.numeric(unlist(strsplit(angStr, ",")));

t1$alox.X=a[1]
t1$alox.Y=a[2]

NT=data.table(t1, key="A")
dt=NT[, list (T=T, X.1=c(0, diff(X)), Y.1=c(0, diff(Y)), X.2=X-alox.X,  
              Y.2=Y-alox.Y), by=c("A") ]

dt[, X.2 := c(0, X.2[2:.N]), by = A]
dt[, Y.2 := c(0, Y.2[2:.N]), by = A]

dtf=dt[, list(scalar=((X.1*X.2)+(Y.1*Y.2)), mag=(sqrt(X.1^2+Y.1^2)*sqrt(X.2^2+Y.2^2))), by=c("A", "T")]
dtf$V=c(0, sqrt(diff(t1$X)^2 + diff(t1$Y)^2))
dtf$ang=deg*(acos(dtf$scalar/dtf$mag))
dtf$angs=sin(dtf$ang)
dtf$ang2=deg*(cos(dtf$scalar/dtf$mag))
dtf$ang3=deg*(sin(dtf$scalar/dtf$mag))
dtf$ang4=deg*(asin(dtf$scalar/dtf$mag))


library(ggplot2)
ggplot(k[k$A==0,],aes(x=X,y=Y))+
  geom_path()+geom_point(colour="red")+coord_fixed()