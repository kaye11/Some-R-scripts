
library(data.table)

#make a new factor
count$reptreatbin <- as.factor(paste(count$reptreat, count$Bin, sep = "-"))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalization
count2=count
count2$CellsN=NA
k=split(count, count$treatment)
countnorm <- lapply(k, function (x) normalize (x[, c("Cells")]))
count2$CellsN=unsplit(countnorm, count$treatment)

#standardization

count2$CellsS=NA
k=split(count2, count2$treatment)
countstd <- lapply(k, function (x) scale(x[,c("Cells")], center=T, scale=T))
count2$CellsS=unsplit(countstd, count2$treatment)

#baselining
NT<-data.table(count2, key=c("reptreatbin"))

t1=NT[,list(treatment=treatment, Bin=Bin, T=T, reptreat=reptreat, CellsBase=(Cells-Cells[1])), by=c("reptreatbin")]

countbase <- t1

source("summarySE.R")
countnormsum <- summarySE(count2, measurevar="CellsN", groupvars=c("T", "treatment", "Bin"))
countstdsum <- summarySE(count2, measurevar="CellsS", groupvars=c("T", "treatment", "Bin"))

countbasesum <- ddply(countwithbase, c("T", "treatment", "Bin"), summarise,
                 N    = length(CellsBase),
                 mean = mean(CellsBase),
                 sd   = sd(CellsBase),
                 se   = sd / sqrt(N))


#ggplot
ggplot(data=countnormsum, aes(x=T, y=CellsN, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsN-se, ymax=CellsN+se), width=8, size=1) + facet_grid(Bin~., scales="free") # normalization is better visually


ggplot(data=countstdsum, aes(x=T, y=CellsS, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=CellsS-se, ymax=CellsS+se), width=5, size=1) + facet_grid(Bin~., scales="free")

ggplot(data=countnormsum, aes(x=T, y=CellsN, shape=treatment, color=treatment))+geom_smooth(method="loess", formula=y~x)+ 
  facet_grid(Bin~., scales="free")


ggplot(data=countbasesum, aes(x=T, y=mean, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=10, size=1) + facet_grid(Bin~., scales="free")

ggplot(data=countbasesum, aes(x=T, y=mean, shape=treatment, color=treatment))+geom_smooth(method="loess", formula=y~x)+ 
  facet_grid(Bin~., scales="free")

ggplot(data=countbase, aes(x=T, y=CellsBase, shape=treatment, color=treatment)) + geom_point(size=5)+ 
  facet_grid(Bin~., scales="free") + geom_line(aes(y=fitted), data=countbase)

#as factor
countnormsum$T.factor = as.factor(countnormsum$T)
countstdsum$T.factor = as.factor(countstdsum$T)
count2$T.factor = as.factor(count2$T)

#ggplot boxplot
qplot(T.factor,CellsN, color = treatment, data = count2,  geom = "boxplot") + facet_wrap(treatment~Bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))

qplot(T.factor,CellsS, color = treatment, data = count2,  geom = "boxplot") + facet_wrap(treatment~Bin) +
  stat_smooth (method="loess", formula=y~x, size=1, aes(group=1))
