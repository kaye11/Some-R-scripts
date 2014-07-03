library(data.table)
library(MASS)
library(fBasics)
library(TeachingDemos)

##use bin data from cons2.R, bind the available data from there. 
#do not source this, just run it DATA FRAME BY DATA FRAME!!!

#st1
st1binA=binA
st1binB=binB
st1binC=binC
st1out=outbin

st1binA$bin=c("binA")
st1binB$bin=c("binB")
st1binC$bin=c("binC")
st1out$bin=c("outbin")

st1=rbind (st1binA, st1binB, st1binC, st1out)

write.table(st1, "d:/Karen's/PhD/R program/Processed_data/points/st1binned.csv", 
            sep=";", col.names=T, row.names=F)


#st2
st2binA=binA
st2binB=binB
st2binC=binC
st2out=outbin

st2binA$bin=c("binA")
st2binB$bin=c("binB")
st2binC$bin=c("binC")
st2out$bin=c("outbin")

st2=rbind (st2binA, st2binB, st2binC, st2out)

write.table(st2, "d:/Karen's/PhD/R program/Processed_data/points/st2binned.csv", 
            sep=";", col.names=T, row.names=F)

#st3
st3binA=binA
st3binB=binB
st3binC=binC
st3out=outbin

st3binA$bin=c("binA")
st3binB$bin=c("binB")
st3binC$bin=c("binC")
st3out$bin=c("outbin")

st3=rbind (st3binA, st3binB, st3binC, st3out)

write.table(st3, "d:/Karen's/PhD/R program/Processed_data/points/st3binned.csv", 
            sep=";", col.names=T, row.names=F)

#binding (st)
stbinA=data.frame(bin=c("binA"), rbind (st1binA, st2binA, st3binA))
stbinB=data.frame(bin=c("binB"), rbind (st1binB, st2binB, st3binB))
stbinC=data.frame(bin=c("binC"), rbind (st1binC, st2binC, st3binC))
stout=data.frame(bin=c("outbin"), rbind (st1out, st2out, st3out))

st=rbind(stbinA, stbinB, stbinC, stout)

#a problem, A is not unique in st so you should compute each parameter separately first

#control
conbinA=data.frame(bin=c("binA"), binA)
conbinB=data.frame(bin=c("binB"), binB)
conbinC=data.frame(bin=c("binC"), binC)
conout=data.frame(bin=c("outbin"), outbin)

con=data.frame(rbind(conbinA, conbinB, conbinC, conout))

##in case something happens export your data
#for st
write.table(st, "d:/Karen's/PhD/R program/Processed_data/points/st.csv", 
            sep=";", col.names=T, row.names=F)

#for control (never name a file name con, it is reserved by windows and will never work)
write.table (con, "d:/Karen's/PhD/R program/Processed_data/points/control.csv", 
            sep=";", col.names=T, row.names=F)

##test the frequency of track change and speed (by data frames [st1, st2, st3], 
#by bin)

#frequency of track change (angle change) by bin
#in case you lose your data, data is saved in trackdata/points with keyword binned :),
#con is saved as control
NT <- data.table(st1, key="bin")
alox1=NT[, list(ang=length(unique(thetadeg)), len=length(T), Vm=mean(V)), by=c("A", "bin")]

NT = data.table(st2)
alox2=NT[, list(ang=length(unique(thetadeg)), len=length(T), Vm=mean(V)), by=c("A", "bin")]

NT = data.table(st3)
alox3=NT[, list(ang=length(unique(thetadeg)), len=length(T), Vm=mean(V)), by=c("A", "bin")]

alox=rbind(alox1, alox2, alox3)
alox$freq=alox$ang/alox$len

NT = data.table(con)
cont=NT[, list(ang=length(unique(thetadeg)), len=length(T), Vm=mean(V)), by=c("A", "bin")]
cont$freq=cont$ang/cont$len

alox$cond=c("Si")
cont$cond=c("Cont")

ns=rbind(alox, cont)

##drop outbin from data set
nso=ns[bin!= "outbin",]

##save nso and ns

write.table (ns, "d:/Karen's/PhD/R program/Processed_data/points/ns.csv", 
             sep=";", col.names=T, row.names=F)

write.table (nso, "d:/Karen's/PhD/R program/Processed_data/points/nso.csv", 
             sep=";", col.names=T, row.names=F)
#boxplot
qplot(cond, Vm, color = cond, data = nso,  geom = "boxplot") + 
  facet_wrap (~bin, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Velocity (um/sec)"))

qplot(cond, freq, color = cond, data = nso,  geom = "boxplot") + 
  facet_wrap (~bin, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Turning rate"))

##histogram checking
ggplot(data = nso, aes(x = Vm)) + geom_histogram(binwidth = diff(range(nso$Vm))/30)+ 
  facet_wrap(~bin*cond, scale="free", nrow=3)

truehist(nso$Vm, main="Histogram of sample")

##normality test, kurtosis, skewness, homogeneity of variances (barlett)
bartlett.test(Vm*freq ~ cond/bin, data=ns) #with outbin
bartlett.test(Vm*freq ~ cond/bin, data=nso) #without outbin

SnowsPenultimateNormalityTest(nso$Vm)
shapiro.test(nso$Vm)

skewness(nso$Vm) ##asymmetry of distribution, if symmetric value should be near 0
kurtosis(nso$Vm) ##flatness of distribution, in normal should be equal to 3

by(ns$freqm, ns$cond, shapiro.test) 
#If p values from normality tests are all greater 0.05 then it is acceptable. 
