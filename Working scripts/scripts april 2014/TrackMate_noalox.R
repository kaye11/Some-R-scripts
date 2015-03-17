##source this! name the file into s1 SOURCEEEE!
library (data.table)
library (ggplot2)

##direct input from trackmate
NT=data.table(s1, key="TRACK_ID")
NT=NT[, list(A=TRACK_ID, X=POSITION_X, Y=POSITION_Y, T=FRAME), by=c("TRACK_ID")]
s1a= NT[order(A, T)]
s1=s1a[,list(T=T, X=X, Y=Y, V=c(0, sqrt(diff(X)^2+diff(Y)^2))), by=c("A")]

##plotting

t1=s1
qplot(X, Y, data = t1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse()+ geom_path(aes(group=factor(A))) 

library(data.table)
library(zoo)

deg<-180/pi


NT<-data.table(t1, key=c("A"))

t1=NT[,list(T=T, X=X, Y=Y, V=V, GD=cumsum(V), ND=sqrt((X-X[1])^2 + (Y-Y[1])^2)), 
      by=c("A")]

t1[,NGDR:=ND/GD]
#t1[,ED:=sqrt((X-X[.N])^2 + (Y-Y[.N])^2), by=A]

t1[,a:=c(NA,(X[2:(.N-1)]),NA),by=A]
t1[,b:=c(NA,(Y[2:(.N-1)]),NA),by=A]
t1[,c:=c(NA,(X[1:(.N-2)]),NA),by=A]
t1[,d:=c(NA,(Y[1:(.N-2)]),NA),by=A]
t1[,e:=c(NA,(X[3:(.N)]),NA),by=A]
t1[,f:=c(NA,(Y[3:(.N)]),NA),by=A]

t1[, scalar:=(a-c)*(a-e)+(b-d)*(b-f)]
t1[, angle:=acos(((a-c)*(a-e)+(b-d)*(b-f))/(sqrt((a-c)^2+(b-d)^2)*sqrt((a-e)^2+(b-f)^2)))*deg]
t1[, angle2:=c(NA, (na.locf(angle [1:(.N-1)])), NA), by=A]
t1[, angs:=sin(angle2)]

##NaNs will be produced when cells are not moving. If scalar is 0 then angle should be 0.
t1$a=NULL
t1$b=NULL
t1$c=NULL
t1$d=NULL
t1$e=NULL
t1$f=NULL
t1$scalar=NULL
t1$angle=NULL
t1$angle2=NULL


##saving data
VN<- readline("What data did you analyse?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/dilutedcells/",VN,".csv")
write.table(t1, Vid, sep=";", col.names=T, row.names=F)


t1=t1[!t1$A=="803", ]
t1=t1[!t1$A=="980", ]
t1=t1[!t1$A=="2623", ]
t1=t1[!t1$A=="7", ]


