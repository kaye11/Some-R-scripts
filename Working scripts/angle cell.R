library(data.table)
library(zoo)

##for computing angles of the cell

NT=data.table(silab1, key="A")

NT$P[NT$P==0]<-NA

NT[,P2:=c(0, (na.locf(P[1:(.N-1)]))), by=A]
NT[,Pdiff:=c(NA, P[2:(.N-1)]- P2[2:(.N-1)], NA), by=A]

NT$Pfin=with(NT, ifelse(Pdiff<(-180), (fin <- Pdiff+360), 
                        ifelse(Pdiff>180, (fin <- Pdiff-360), fin<-Pdiff)))

silab1=NT[,list(T=T, X=X, Y=Y, V=c(0, sqrt(diff(X)^2+diff(Y)^2)), GD=cumsum(V), 
            ND=sqrt((X-X[1])^2 + (Y-Y[1])^2), P=Pfin), 
      by=c("A")]
