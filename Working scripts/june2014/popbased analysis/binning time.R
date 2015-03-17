##binning by time. use spatially binned data 
##using the saved binned data, compute parameters by time range

tm=seq(0, 600, by = 30)
si.binned$time <- cut(si.binned$T, tm, labels=paste(tail(tm, -1L)))

si.binned$time[is.na(si.binned$time)] <- 30 #replace NAs with 30. some data time points have the starting point as NA

NT <- data.table(si.binned, key="bin")
si.alox=unique(NT[, list(Vm=mean(V), ang=length(unique(angs)), len=length(T), dir=mean(angs, na.rm=TRUE), 
                         count=length (unique(A))), 
                  by=c("bin", "time")])

si.alox$freq=si.alox$ang/si.alox$len
si.alox=si.alox[complete.cases(si.alox$dir), ]#remove data with NaNs

con.binned$time <- cut(con.binned$T, tm, labels=paste(tail(tm, -1L)))
con.binned$time[is.na(con.binned$time)] <- 30 #replace NAs with 30. some data time points have the starting point as NA

NT = data.table(con.binned)
con.alox=unique(NT[, list(Vm=mean(V), ang=length(unique(angs)), len=length(T), dir=mean(angs, na.rm=TRUE), 
                          count=length (unique(A))), 
                   by=c("bin", "time")])
con.alox$freq=con.alox$ang/con.alox$len

con.alox=con.alox[complete.cases(con.alox$dir), ]#remove data with NaNs

si.alox$cond=c("Si")
con.alox$cond=c("Cont")

aloxall=rbind(si.alox, con.alox)

##drop outbin from data set
alox=aloxall[bin!= "outbin",]

##save alox and aloxall (2nd edition)

VN<- readline("What data did you analyse? all data! binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(aloxall, Vid, sep=";", col.names=T, row.names=F)

VN<- readline("What data did you analyse? without outbin! data?binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(alox, Vid, sep=";", col.names=T, row.names=F)


