library(data.table)
library(mgcv)
##using the saved binned data, compute parameters by time range

tm=seq(0, 600, by = 30)
st1binned$time <- cut(st1binned$T, tm, labels=paste(head(tm, -1L), tail(tm, -1L), sep="-" ))
NT <- data.table(st1binned, key="bin")
alox1=unique(NT[, list(Vm=mean(V), ang=length(unique(thetadeg)), len=length(T), dir=mean(unique(thetadeg))), 
                by=c("A", "bin", "time")])

st2binned$time <- cut(st2binned$T, tm, labels=paste(head(tm, -1L), tail(tm, -1L), sep="-" ))
NT <- data.table(st2binned, key="bin")
alox2=unique(NT[, list(Vm=mean(V), ang=length(unique(thetadeg)), len=length(T), dir=mean(unique(thetadeg))), 
                by=c("A", "bin", "time")])

st3binned$time <- cut(st3binned$T, tm, labels=paste(head(tm, -1L), tail(tm, -1L), sep="-" ))
NT <- data.table(st3binned, key="bin")
alox3=unique(NT[, list(Vm=mean(V), ang=length(unique(thetadeg)), len=length(T), dir=mean(unique(thetadeg))), 
                by=c("A", "bin", "time")])

alox=rbind(alox1, alox2, alox3)
alox$freq=alox$ang/alox$len

control$time <- cut(control$T, tm, labels=paste(head(tm, -1L), tail(tm, -1L), sep="-" ))
NT = data.table(control)
cont=unique(NT[, list(Vm=mean(V), ang=length(unique(thetadeg)), len=length(T), dir=mean(unique(thetadeg))), 
               by=c("A", "bin", "time")])
cont$freq=cont$ang/cont$len

alox$cond=c("Si")
cont$cond=c("Cont")

aloxall=rbind(alox, cont)

##drop outbin from data set
alox=aloxall[bin!= "outbin",]

##save nst and alox

write.table (aloxall, "d:/Karen's/PhD/R program/Processed_data/points/aloxall.csv", 
             sep=";", col.names=T, row.names=F)

write.table (alox, "d:/Karen's/PhD/R program/Processed_data/points/alox.csv", 
             sep=";", col.names=T, row.names=F)


#boxplot
qplot(cond, Vm, color = cond, data = nsto,  geom = "boxplot") + 
  facet_wrap (~bin+time, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Velocity (um/sec)"))

qplot(cond, freq, color = cond, data = nsto,  geom = "boxplot") + 
  facet_wrap (~bin+time, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Turning rate"))

##histogram checking
ggplot(data = nsto [bin=="binA"], aes(x = Vm)) + geom_histogram(binwidth = diff(range(nsto$Vm))/30)+ 
  facet_wrap(~cond*time, scale="free")

ggplot(data = nsto [bin=="binB"], aes(x = Vm)) + geom_histogram(binwidth = diff(range(nsto$Vm))/30)+ 
  facet_wrap(~cond*time, scale="free")

ggplot(data = nsto [bin=="binC"], aes(x = Vm)) + geom_histogram(binwidth = diff(range(nsto$Vm))/30)+ 
  facet_wrap(~cond*time, scale="free")

by(nsto$Vm [nsto$bin=="binA"], nsto$cond [nsto$bin=="binA"], shapiro.test)
by(nsto$Vm [nsto$bin=="binB"], nsto$cond [nsto$bin=="binB"], shapiro.test)
by(nsto$Vm [nsto$bin=="binC"], nsto$cond [nsto$bin=="binC"], shapiro.test)

##gams
binA<-gam(Vm ~  time,by = as.numeric(cond == "Si") + time,by = as.numeric(cond == "Cont"), 
        data = nsto[nsto$bin == "binA"])

binB<-gam(Vm ~  time,by = as.numeric(cond == "Si") + time,by = as.numeric(cond == "Cont"), 
          data = nsto[nsto$bin == "binB"])

binC<-gam(Vm ~  time,by = as.numeric(cond == "Si") + time,by = as.numeric(cond == "Cont"), 
          data = nsto[nsto$bin == "binC"])

binAA<-gam(Vm ~  cond/bin*time, data = nsto)
