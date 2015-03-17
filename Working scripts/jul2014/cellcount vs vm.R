library(plyr)
library(data.table)
library(ggplot2)
library(Kendall)

##let's start

#per bin
bincount=as.data.frame(na.omit(count(raw.binned, vars=c("cond", "bin"))))
NT <- data.table(raw.binned, key="bin")
rbc=NT[, list(Vm=mean(V, na.rm=TRUE), angmean=mean(angs, na.rm=TRUE)), 
      by=c("cond", "bin")]

perbin=as.data.frame(merge(bincount, rbc, by= c("cond", "bin")))

#per angdir
am=seq(-1.0, 1.0, 0.2)
raw.binned$angdir <- cut(raw.binned$angs, am, include.lowest=T, labels=paste(head(am, -1L), tail (am, -1L), sep="-"))


angcount=as.data.frame(na.omit(count(raw.binned, vars=c("cond", "bin", "angdir"))))
NT <- data.table(raw.binned, key="bin")
rbc=NT[, list(Vm=mean(V, na.rm=TRUE), angmean=mean(angs, na.rm=TRUE)), 
       by=c("cond", "bin", "angdir")]

perang=as.data.frame(merge(angcount, rbc, by= c("cond", "bin", "angdir")))
perang=as.data.frame(na.omit(perang))


#pertime
tm=seq(0, 600, by = 60)
raw.binned$time <- cut(raw.binned$T, tm, labels=paste(tail(tm, -1L)))
raw.binned$time[is.na(raw.binned$time)] <- 60 #replace NAs with 30. some data time points have the starting point as NA


timecount1=as.data.frame(na.omit(count(raw.binned, vars=c("cond", "bin", "angdir", "time"))))
NT <- data.table(raw.binned, key="bin")
rbc=NT[, list(Vm=mean(V, na.rm=TRUE), angmean=mean(angs, na.rm=TRUE)), 
       by=c("cond", "bin", "angdir", "time")]

pertime1=as.data.frame(merge(timecount1, rbc, by= c("cond", "bin", "angdir", "time")))


timecount2=as.data.frame(na.omit(count(raw.binned, vars=c("cond", "bin", "time"))))
NT <- data.table(raw.binned, key="bin")
rbc=NT[, list(Vm=mean(V, na.rm=TRUE), angmean=mean(angs, na.rm=TRUE)), 
       by=c("cond", "bin", "time")]

pertime2=as.data.frame(merge(timecount2, rbc, by= c("cond", "bin", "time")))

##Kendall
#perbin data
Kendall(perbin$Vm [perbin$cond=="Con"], perbin$freq [perbin$cond=="Con"]) #not sig
Kendall(perbin$Vm [perbin$cond=="Si"], perbin$freq [perbin$cond=="Si"]) #not sig

#perang time
Kendall(perang$Vm [perang$cond=="Con"], perang$freq [perang$cond=="Con"]) #sig (tau=-0.219)
Kendall(perang$Vm [perang$cond=="Si"], perang$freq [perang$cond=="Si"]) #sig (tau=-0.316)

Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="binA"], 
        perang$freq [perang$cond=="Con" & perang$bin=="binA"]) #not sig
Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="binB"], 
        perang$freq [perang$cond=="Con" & perang$bin=="binB"]) #not sig
Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="binC"], 
        perang$freq [perang$cond=="Con" & perang$bin=="binC"]) #not sig
Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="binD"], 
        perang$freq [perang$cond=="Con" & perang$bin=="binD"]) #not sig
Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="binE"], 
        perang$freq [perang$cond=="Con" & perang$bin=="binE"]) #not sig
Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="binF"], 
        perang$freq [perang$cond=="Con" & perang$bin=="binF"]) #not sig
Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="binG"], 
        perang$freq [perang$cond=="Con" & perang$bin=="binG"]) #not sig
Kendall(perang$Vm [perang$cond=="Con" & perang$bin=="outbin"], 
        perang$freq [perang$cond=="Con" & perang$bin=="outbin"]) #sig (tau=0.539)


Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="binA"], 
        perang$freq [perang$cond=="Si" & perang$bin=="binA"]) #not sig
Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="binB"], 
        perang$freq [perang$cond=="Si" & perang$bin=="binB"]) #not sig
Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="binC"], 
        perang$freq [perang$cond=="Si" & perang$bin=="binC"]) #not sig
Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="binD"], 
        perang$freq [perang$cond=="Si" & perang$bin=="binD"]) #not sig
Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="binE"], 
        perang$freq [perang$cond=="Si" & perang$bin=="binE"]) #not sig
Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="binF"], 
        perang$freq [perang$cond=="Si" & perang$bin=="binF"]) #not sig
Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="binG"], 
        perang$freq [perang$cond=="Si" & perang$bin=="binG"]) #not sig
Kendall(perang$Vm [perang$cond=="Si" & perang$bin=="outbin"], 
        perang$freq [perang$cond=="Si" & perang$bin=="outbin"]) #not sig


#pertime and angdir
Kendall(pertime1$Vm [pertime1$cond=="Con"], pertime1$freq [pertime1$cond=="Con"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Si"], pertime1$freq [pertime1$cond=="Si"]) #sig (tau=-0.284)

Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="binA"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="binA"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="binB"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="binB"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="binC"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="binC"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="binD"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="binD"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="binE"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="binE"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="binF"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="binF"]) #sig (tau=0.148)
Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="binG"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="binG"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Con" & pertime1$bin=="outbin"], 
        pertime1$freq [pertime1$cond=="Con" & pertime1$bin=="outbin"]) #not sig


Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="binA"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="binA"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="binB"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="binB"]) #sig (tau=-0.22)
Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="binC"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="binC"]) #not sig
Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="binD"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="binD"]) #sig (tau=-0.293)
Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="binE"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="binE"]) #sig (tau=0.207)
Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="binF"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="binF"]) #sig (tau=-0.187)
Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="binG"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="binG"]) #sig (tau=-0.324)
Kendall(pertime1$Vm [pertime1$cond=="Si" & pertime1$bin=="outbin"], 
        pertime1$freq [pertime1$cond=="Si" & pertime1$bin=="outbin"]) #sig (tau=-0.357)
