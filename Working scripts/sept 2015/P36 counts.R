
#read data
A4Ge06 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/counts P36/ A4Ge06 .csv", sep=";")
B2Si08 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/counts P36/ B2Si08 .csv", sep=";")
B4Control011 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/counts P36/ B4Control011 .csv", sep=";")
B4Si011 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/counts P36/ B4Si011 .csv", sep=";")
C2Ge013 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/counts P36/ C2Ge013 .csv", sep=";")
B3Si010 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/counts P36/B3Si010 .csv", sep=";")

Si <- rbind (B2Si08, B4Si011, B3Si010)
Con <- B4Control011
Ge <- rbind(A4Ge06, C2Ge013)

#whole data set
P36 <- rbind (Con, Si, Ge)

#summary
source("summarySE.R")

#P36_summarycount <- summarySE(P36, measurevar = "Count", groupvars = c("bin", "cond", "T"), .drop=FALSE, na.rm=FALSE)

P36_summarycount <- ddply(P36, c("T", "bin", "cond"), summarise,
                N    = length(Count),
                mean = mean(Count),
                sd   = sd(Count),
                se   = sd / sqrt(N))

#ggplot
ggplot(data=P36_summarycount, aes(x=T, y=mean, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5, size=1) + facet_grid(bin~., scales="free")

ggplot(data=P36_summarycount, aes(x=T, y=mean, fill=cond)) + geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + facet_grid(~bin)

ggplot(data=P36_summarycount, aes(x=T, y=mean, color=cond))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=2, size=1)+
  geom_line()+geom_point()+facet_grid(~bin)

ggplot(data=P36_summarycount, aes(x=T, y=mean, color=cond, shape=cond))+geom_smooth()+geom_point()+ facet_grid(bin~., scales="free")

p36_10 <- subset(P36, P36$T<11, )

p36_10$T.factor <- as.factor(p36_10$T)

qplot(T.factor,Count, color = cond, data = p36_10,  geom = "boxplot") + facet_wrap(cond~bin, scales="free") 
