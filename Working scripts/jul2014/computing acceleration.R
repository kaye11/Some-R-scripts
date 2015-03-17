##NOTE: THE PATH FILE IS DIFFERENT FROM USUAL! 
library(data.table)

t1=con.raw
t1<-data.table(con.raw, key=c("A"))

t1[,a:=c(NA,(V[2:(.N)])),by=A]
t1[,b:=c(NA,(V[1:(.N-1)])),by=A]

t1[, Ac:=a-b]
t1$a=NULL
t1$b=NULL
t11=as.data.frame(t1)

write.table (t11, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/control_raw_Ac.csv", 
             sep=";", col.names=T, row.names=F)


t1=si.raw
t1<-data.table(si.raw, key=c("A"))

t1[,a:=c(NA,(V[2:(.N)])),by=A]
t1[,b:=c(NA,(V[1:(.N-1)])),by=A]

t1[, Ac:=a-b]
t1$a=NULL
t1$b=NULL
t11=as.data.frame(t1)

write.table (t11, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/si_raw_Ac.csv", 
             sep=";", col.names=T, row.names=F)


