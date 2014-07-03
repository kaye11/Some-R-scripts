library (data.table)
library (ggplot2)

NT=data.table(s1, key="A")
s1a= NT[order(A, T)]

s1=t3a[,list(T=T, X=X, Y=Y, V=c(0, sqrt(diff(X)^2+diff(Y)^2))), by=c("A")]


##direct input from trackmate
NT=data.table(s1, key="TRACK_ID")
NT=NT[, list(A=TRACK_ID, X=POSITION_X, Y=POSITION_Y, T=POSITION_T), by=c("TRACK_ID")]
s1a= NT[order(A, T)]
s1=s1a[,list(T=T, X=X, Y=Y, V=c(0, sqrt(diff(X)^2+diff(Y)^2))), by=c("A")]