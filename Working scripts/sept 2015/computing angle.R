##parameter computations
beadX<-as.numeric(readline("X position of the Bead?"))
beadY<-as.numeric(readline("Y position of the Bead?"))

library(data.table)
library(zoo)

deg<-180/pi


NT<-data.table(t1, key=c("A"))
NT[, V := c(0, V[2:(.N-1)], 0), by = A]

t1=NT[,list(T=T, X=X, Y=Y, V=c(0, sqrt(diff(X)^2+diff(Y)^2)), GD=cumsum(V), ND=sqrt((X-X[1])^2 + (Y-Y[1])^2)), 
      by=c("A")]

t1[,NGDR:=ND/GD]
#t1[,ED:=sqrt((X-X[.N])^2 + (Y-Y[.N])^2), by=A]


t1[,a:=c(NA,(X[2:(.N-1)]),NA),by=A]
t1[,b:=c(NA,(Y[2:(.N-1)]),NA),by=A]
t1[,c:=c(NA,(X[1:(.N-2)]),NA),by=A]
t1[,d:=c(NA,(Y[1:(.N-2)]),NA),by=A]

t1[, scalar:=(a-c)*(a-beadX)+(b-d)*(b-beadY)]
t1[, angle:=acos(((a-c)*(a-beadX)+(b-d)*(b-beadY))/(sqrt((a-c)^2+(b-d)^2)*sqrt((a-beadX)^2+(b-beadY)^2)))*deg]
t1[, angle2:=c(NA, (na.locf(angle [1:(.N-1)])), NA), by=A]
t1[, angs:=sin(angle2)]

t1$a=NULL
t1$b=NULL
t1$c=NULL
t1$d=NULL
t1$scalar=NULL
t1$angle=NULL
t1$angle2=NULL

##saving data
VN<- readline("What data did you analyse?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/",VN,".csv")
write.table(t1, Vid, sep=";", col.names=T, row.names=F)



