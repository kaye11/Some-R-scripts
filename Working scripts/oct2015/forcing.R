#for max number of tracks with the start of each track forced to 0
t1=vm4.control.raw
NT = data.table(t1)
NT2=NT[, time := seq(from = 0L, by = 1L, length.out = .N), by = A]

#con=p36.control.forced
con=as.data.frame(NT2)
con$A=as.numeric(con$A)
con.count2=ddply(con,~time,summarise,Con=(length(A)))

VN<- readline("What data did you analyse? SPECIFY DATA TYPE (control):")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/forced data/vm4_angsnotcorrect/",VN,".csv")
write.table(con, Vid, sep=";", col.names=T, row.names=F)


t2=vm4.si.raw
NT = data.table(t2)
NT2=NT[, time := seq(from = 0L, by = 1L, length.out = .N), by = A]

#si=p36.si.forced
si=as.data.frame(NT2)
si$A=as.numeric(si$A)
si.count2=ddply(si,~time,summarise,si=(length(A)))

VN<- readline("What data did you analyse? SPECIFY DATA TYPE (si):")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/forced data/vm4_angsnotcorrect/",VN,".csv")
write.table(si, Vid, sep=";", col.names=T, row.names=F)



#data binding
qplot(x=time, y=Con, data=con.count2, geom="point")
qplot(x=time, y=si, data=si.count2, geom="point")


#con.count1=ddply(con, .(time, bin), summarise,Con=(length(A)))
