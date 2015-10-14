
control <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/old_data/ day3 control-001 .csv", sep=";")
si <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/old_data/ day3 si-002 .csv", sep=";")

control$cond="Control"
si$cond="dSi"

binned <- rbind(control, si)

source("summarySE.R")
angsum<- summarySE(binned, measurevar="angs", groupvars=c("cond", "bin", "time"), na.rm=TRUE)

qplot(time,angs, data = binned,  geom = "boxplot", color=cond) + facet_grid(cond~bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

ggplot(data=angsum, aes(x=time, y=angs, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=0.2, size=1) + facet_grid(.~bin) + geom_hline(yintercept=0)

ggplot(data = binned, aes(x=T,y=angs, color=cond))+ stat_smooth(method="gam", formula=y~s(x, k=10), size=2, se=TRUE)+ facet_grid(.~bin)

p36.control <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/p36/ P36 control-011 .csv", sep=";")
p36.si <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/p36/ P36 si-012 .csv", sep=";")

p36.control$cond="Control"
p36.si$cond="dSi"

p36.binned <- rbind(p36.control, p36.si)

p36.angsum<- summarySE(p36.binned, measurevar="angs", groupvars=c("cond", "bin", "time"), na.rm=TRUE)


qplot(time,angs, data = p36.binned,  geom = "boxplot", color=cond) + facet_grid(cond~bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

ggplot(data=p36.angsum, aes(x=time, y=angs, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=0.2, size=1) + facet_grid(.~bin) + geom_hline(yintercept=0)

ggplot(data = p36.binned, aes(x=T,y=angs, color=cond))+ stat_smooth(method="gam", formula=y~s(x, k=10), size=2, se=TRUE)+ facet_grid(.~bin)

binned2= rbind(binned, p36.binned)

angsum2<- summarySE(binned2, measurevar="angs", groupvars=c("cond", "bin", "time"), na.rm=TRUE)

qplot(time,angs, data = binned2,  geom = "boxplot", color=cond) + facet_grid(cond~bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

ggplot(data=angsum2, aes(x=time, y=angs, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=0.2, size=1) + facet_grid(.~bin) + geom_hline(yintercept=0)

ggplot(data = binned, aes(x=T,y=angs, color=cond))+ stat_smooth(method="gam", formula=y~s(x, k=10), size=2, se=TRUE)+ facet_grid(.~bin)





source("summarySE.R")
Vsum<- summarySE(binned, measurevar="V", groupvars=c("cond", "bin", "time"), na.rm=TRUE)
Vsum2<- summarySE(binned, measurevar="V", groupvars=c("cond", "time"), na.rm=TRUE)

Vsum=Vsum [Vsum$time!="0", ]  
qplot(time,V, data = binned,  geom = "boxplot", color=cond) + facet_grid(cond~bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

ggplot(data=Vsum, aes(x=time, y=V, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=V-se, ymax=V+se), width=0.4, size=1) + facet_grid(bin~cond)

ggplot(data = binned, aes(x=T,y=V, color=cond))+ stat_smooth(method="gam", formula=y~s(x, k=3), size=2, se=TRUE)+ facet_grid(.~bin)


p36.Vsum<- summarySE(p36.binned, measurevar="V", groupvars=c("cond", "bin", "time"), na.rm=TRUE)

p36.Vsum=p36.Vsum [p36.Vsum$time!="0", ]  

qplot(time,V, data = p36.binned,  geom = "boxplot", color=cond) + facet_grid(cond~bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

ggplot(data=p36.Vsum, aes(x=time, y=V, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=V-se, ymax=V+se), width=0.2, size=1) + facet_grid(cond~bin)+stat_smooth(aes(group=1)) 

ggplot(data = p36.binned, aes(x=T,y=V, color=cond))+ stat_smooth(method="gam", formula=y~s(x, k=5), size=2, se=TRUE)+ facet_grid(.~bin)
ggplot(data = p36.Vsum, aes(x=time,y=V, color=cond))+ stat_smooth(aes(group=cond))+ facet_grid(.~bin)


Vsum.comb<- summarySE(binned2, measurevar="V", groupvars=c("cond", "bin", "time"), na.rm=TRUE)

qplot(time,V, data = binned2,  geom = "boxplot", color=cond) + facet_grid(cond~bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

ggplot(data=Vsum.comb, aes(x=time, y=V, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=V-se, ymax=V+se), width=0.2, size=1) + facet_grid(.~bin) 

ggplot(data = binned, aes(x=T,y=angs, color=cond))+ stat_smooth(method="gam", formula=y~s(x, k=10), size=2, se=TRUE)+ facet_grid(.~bin)

