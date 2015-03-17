
#per angdir
am=seq(0, 1.0, 0.1)
raw.binned$ng <- cut(raw.binned$NGDR, am, include.lowest=T, labels=paste(head(am, -1L), tail (am, -1L), sep="-"))


ngdrcount=as.data.frame(na.omit(count(raw.binned, vars=c("cond", "bin", "ng"))))
NT <- data.table(raw.binned, key="bin")
rbc=NT[, list(Vm=mean(V, na.rm=TRUE), angmean=mean(angs, na.rm=TRUE)), 
       by=c("cond", "bin", "ng")]

perngdr=as.data.frame(merge(ngdrcount, rbc, by= c("cond", "bin", "ng")))
perngdr=as.data.frame(na.omit(perngdr))

ggplot(data=perngdr, aes (x=ng, y=freq, colour=cond, fill=cond)) + facet_wrap (~bin, ncol=2) + 
  geom_bar(stat="identity", position=position_dodge())


ngdrcount2=as.data.frame(na.omit(count(raw.binned, vars=c("cond", "ng"))))
NT <- data.table(raw.binned, key="bin")
rbc=NT[, list(Vm=mean(V, na.rm=TRUE), angmean=mean(angs, na.rm=TRUE)), 
       by=c("cond", "ng")]

perngdr2=as.data.frame(merge(ngdrcount2, rbc, by= c("cond", "ng")))
perngdr2=as.data.frame(na.omit(perngdr2))

ggplot(data=perngdr2, aes (x=ng, y=freq, colour=cond, fill=cond)) + 
  geom_bar(stat="identity", position=position_dodge())


#pertime
tm=seq(0, 600, by = 60)
raw.binned$time <- cut(raw.binned$T, tm, labels=paste(tail(tm, -1L)))
raw.binned$time[is.na(raw.binned$time)] <- 60 #replace NAs with 60. some data time points have the starting point as NA


timecount1=as.data.frame(na.omit(count(raw.binned, vars=c("cond", "bin", "ng", "time"))))
NT <- data.table(raw.binned, key="bin")
rbc=NT[, list(Vm=mean(V, na.rm=TRUE), angmean=mean(angs, na.rm=TRUE)), 
       by=c("cond", "bin", "ng", "time")]

pertime1=as.data.frame(merge(timecount1, rbc, by= c("cond", "bin", "ng", "time")))

 