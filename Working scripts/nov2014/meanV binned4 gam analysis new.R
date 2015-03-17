##binning by T. use spatially binned data 
##using the saved binned data, compute parameters by T range

tm=seq(0, 600, by = 30)
si.binned$T <- cut(si.binned$T, tm, include.lowest=T, labels=paste(head(tm, -1L), tail (tm, -1L), sep="-"))
con.binned$T <- cut(con.binned$T, tm, include.lowest=T, labels=paste(head(tm, -1L), tail (tm, -1L), sep="-"))

si.binned$cond="Si"
con.binned$cond="Con"
#si.binned$angle=NULL
#si.binned$angle2=NULL
#si.binned$scalar=NULL
#con.binned$angle=NULL
#con.binned$angle2=NULL
#con.binned$scalar=NULL

VN<- readline("What data did you analyse? all data! binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(si.binned, Vid, sep=";", col.names=T, row.names=F)

VN<- readline("What data did you analyse? without outbin! data?binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(con.binned, Vid, sep=";", col.names=T, row.names=F)

all.binned=rbind (si.binned, con.binned)

VN<- readline("What data did you analyse? without outbin! data?binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(all.binned, Vid, sep=";", col.names=T, row.names=F)

dfc <- summarySE(all.binned, measurevar="V", na.rm=TRUE, groupvars=c("cond","bin", "T"))
dfc2 <- summarySE(all.binned, measurevar="Vlog", na.rm=TRUE, groupvars=c("cond","bin", "T"))



all.binned$T=as.numeric(all.binned$T)
all.binned$A=as.factor(all.binned$A)
all.binned$Vlog=log(all.binned$V+1)

binned=all.binned[all.binned$bin!= "outbin",]
binned$time=binned$time*30
binned$binn=as.numeric(binned$bin)

levels(binned$A)[levels(binned$A)=="0"] <- "5000"

binned$An=as.numeric(binned$A)
binned$condn=binned$cond

binned$condn=as.numeric(levels(binned$cond)[levels(binned$cond)=="Si"] <- "1")
binned$condn=as.numeric(levels(binned$cond)[levels(binned$cond)=="Con"] <- "0")


## GAM models for Bins

#transformed data
BinA<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con")) +
            s(T,by = as.numeric(cond == "Si")), data = binned[binned$bin == "binA",])  
BinB<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con")) +
            s(T,by = as.numeric(cond == "Si")), data = binned[binned$bin == "binB",])  
BinC<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con")) +
            s(T,by = as.numeric(cond == "Si")), data = binned[binned$bin == "binC",])  

AIC(BinA, BinB, BinC)

#non transformed data
BinAnt<-gam(V ~  s(T,by = as.numeric(cond == "Con")) +
            s(T,by = as.numeric(cond == "Si")), data = binned[binned$bin == "binA",])  
BinBnt<-gam(V ~  s(T,by = as.numeric(cond == "Con")) +
            s(T,by = as.numeric(cond == "Si")), data = binned[binned$bin == "binB",])  
BinCnt<-gam(V ~  s(T,by = as.numeric(cond == "Con")) +
            s(T,by = as.numeric(cond == "Si")), data = binned[binned$bin == "binC",])  

AIC(BinA, BinB, BinC, BinAnt, BinBnt, BinCnt)


#boxplot
qplot(cond, V, color = cond, data =binned,  geom = "boxplot")
qplot(cond, Vlog, color = cond, data = binned,  geom = "boxplot")

#test binA
A1<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cs") +
            s(T,by = as.numeric(cond == "Si"), bs="cs"), data = binned[binned$bin == "binA",])

A2<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr") +
          s(T,by = as.numeric(cond == "Si"), bs="cr"), data = binned[binned$bin == "binA",])  

A3<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=20) +
          s(T,by = as.numeric(cond == "Si"), bs="cr", k=20), data = binned[binned$bin == "binA",])  

A4<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=15) +
          s(T,by = as.numeric(cond == "Si"), bs="cr", k=15), data = binned[binned$bin == "binA",])  

A5<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=12) +
          s(T,by = as.numeric(cond == "Si"), bs="cr", k=12), data = binned[binned$bin == "binA",])  

A6 <- gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=10) +
            s(T,by = as.numeric(cond == "Si"), bs="cr", k=10), data = binned[binned$bin == "binA",])  

A7 <- gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=8) +
            s(T,by = as.numeric(cond == "Si"), bs="cr", k=8), data = binned[binned$bin == "binA",])  

A8 <- gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=5) +
            s(T,by = as.numeric(cond == "Si"), bs="cr", k=5), data = binned[binned$bin == "binA",])  

A9 <- gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=3) +
            s(T,by = as.numeric(cond == "Si"), bs="cr", k=3), data = binned[binned$bin == "binA",])  

AIC(BinA, A1, A2, A3, A4, A5, A6, A7, A8, A9)
#use bs="cr", k=20

##Correlation

#no correlation and collinearity observed in Bin A)
binAdata=subset(binned, bin == "binA")

#A3 no random factor
AR<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=20) +
          s(T,by = as.numeric(cond == "Si"), bs="cr", k=20) + s(A, bs="re"), data = binned[binned$bin == "binA",])  

AR2<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=20) +
          s(T,by = as.numeric(cond == "Si"), bs="cr", k=20) + s(A, bs="re") + cond, data = binned[binned$bin == "binA",])  

AR3<-gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=20) +
           s(T,by = as.numeric(cond == "Si"), bs="cr", k=20) + s(A, bs="re") + factor(cond), data = binned[binned$bin == "binA",])  

AIC(A3, AR, AR2, AR3)

AR4<-gam(Vlog ~ cond + s (T, bs="cr", k=20) + s(A, bs="re"), data = binned[binned$bin == "binA",])  

AR5<-gamm(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=20) +
          s(T,by = as.numeric(cond == "Si"), bs="cr", k=20) + T, random = list (A = ~1), data = binned[binned$bin == "binA",])  

#simplest form

SF <- gam (Vlog ~ T + bin + cond, data=binned)

SF1 <- gam (Vlog ~ s(T) + bin + cond, data=binned)

SF2 <- gam (Vlog ~ s(T, bs="cr", k=20) + bin + cond, data=binned)

SF3 <- gam (Vlog ~ s(T, bs="cr", k=50) + bin + cond, data=binned)

SF4 <- gam (Vlog ~ s(T, bs="cr", k=600) + bin + cond, data=binned)

AIC (SF, SF1, SF2, SF3, SF4) # k=50 is okay SF3

#making it more complicated

C1 <- gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=40) +
             s(T,by = as.numeric(cond == "Si"), bs="cr", k=40) + bin + cond, data = binned) 

C2 <- gam(Vlog ~  s(T,by = as.numeric(cond == "Con"), bs="cr", k=41) +
            s(T,by = as.numeric(cond == "Si"), bs="cr", k=40) + bin + cond + s(A, bs="re"), data = binned) 

lmc <- lmeControl(niterEM = 5000, msMaxIter = 1000)

f1 <- Vlog ~  s (T) + cond
f2 <- Vlog ~  s (T) + cond + bin
f3 <- Vlog ~  s(time,by = as.numeric(cond == "Con")) +   s(time,by = as.numeric(cond == "Si"))  

f1.A <- gamm(f1, random = list (A =~ 1),method = "REML", control = lmc, data = binned)
f2.A <- gamm(f2, random = list (A =~ 1),method = "REML", control = lmc, data = binned)
f3.A <- gamm(f3, random = list (A =~ 1),method = "REML", control = lmc, data = binned)


f1.B <- gamm(f1, random = list (A =~ 1),weights = varIdent(form =~1 | A), method = "REML", control = lmc, data = binned)
f2.B <- gamm(f2, random = list (A =~ 1),method = "REML", control = lmc, data = binned)


AIC(f1.A$lme, f2.A$lme, C2)

tgamm <- gamm (f2, correlation = corCompSymm(form=~time|bin/A), data=binned, control=lmc)
tgamm2 <- gamm (f2, correlation = corCompSymm(form=~A), data=binned, control=lmc)
tgamm <- gamm (f2, correlation = corCompSymm(form=~time|bin/A), data=binned, control=lmc)

