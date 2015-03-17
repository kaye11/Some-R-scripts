#This script is for combining and testing data set for diluted cells (si, control and mot)

library(ggplot2)
library(data.table)
library(gdata)
library(reshape)
library(plotrix)
library(Kendall)

##for motility data
NT = data.table(m1, key="A")
NT2= data.table(m1.2, key="A")
NT3= data.table(m2, key="A")

mot1.1 <- NT[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                    NGDR=mean(NGDR, na.rm=TRUE), dir=mean(angs, na.rm=TRUE)), by=c("A")] 
mot1.2 <- NT2[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                     NGDR=mean(NGDR, na.rm=TRUE), dir=mean(angs, na.rm=TRUE)), by=c("A")] 
mot2 <- NT2[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                     NGDR=mean(NGDR, na.rm=TRUE),  dir=mean(angs, na.rm=TRUE)), by=c("A")]

mot1.1[,freq:=(ang/len),by=A]
mot1.2[,freq:=(ang/len),by=A]
mot2[,freq:=(ang/len),by=A]
mot1.1[,cond:=c("mot"),by=A]
mot1.2[,cond:=c("mot"),by=A]
mot2[,cond:=c("mot"),by=A]

mot=rbind(mot1.1, mot1.2, mot2)

#for Si cells

NT = data.table(d2, key="A")
NT2= data.table(d6, key="A")
NT3= data.table(d7, key="A")

dil2 <- NT[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                    NGDR=mean(NGDR, na.rm=TRUE),  dir=mean(angs, na.rm=TRUE)), by=c("A")] 
dil6 <- NT2[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                     NGDR=mean(NGDR, na.rm=TRUE), dir=mean(angs, na.rm=TRUE)), by=c("A")]
dil7 <- NT3[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                   NGDR=mean(NGDR, na.rm=TRUE),  dir=mean(angs, na.rm=TRUE)), by=c("A")]

dil2[,freq:=(ang/len),by=A]
dil6[,freq:=(ang/len),by=A]
dil7[,freq:=(ang/len),by=A]
dil2[,cond:=c("si"),by=A]
dil6[,cond:=c("si"),by=A]
dil7[,cond:=c("si"),by=A]


si=rbind(dil2, dil6, dil7)

##for control cells

NT = data.table(c1, key="A")
NT2= data.table(c2, key="A")


con1 <- NT[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                  NGDR=mean(NGDR, na.rm=TRUE),  dir=mean(angs, na.rm=TRUE)), by=c("A")] 
con2 <- NT2[, list(ang=length(unique(angs)), len=length(T), Vm=mean(V), 
                   NGDR=mean(NGDR, na.rm=TRUE),  dir=mean(angs, na.rm=TRUE)), by=c("A")]

con1[,freq:=(ang/len),by=A]
con2[,freq:=(ang/len),by=A]
con1[,cond:=c("con"),by=A]
con2[,cond:=c("con"),by=A]

con=rbind(con1, con2)



#combining the two data sets
all=rbind(mot, con, si)

Vm.summary <- summarySE(all, measurevar="Vm", groupvars="cond")
dir.summary <- summarySE(all, measurevar="dir", groupvars="cond")
freq.summary <- summarySE(all, measurevar="freq", groupvars="cond")
NGDR.summary <- summarySE(all, measurevar="NGDR", groupvars="cond")



ggplot(Vm.summary, aes(x=cond, y=Vm, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Vm-se, ymax=Vm+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

ggplot(dir.summary, aes(x=cond, y=dir, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=dir-se, ymax=dir+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

ggplot(freq.summary, aes(x=cond, y=freq, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=freq-se, ymax=freq+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

ggplot(NGDR.summary, aes(x=cond, y=NGDR, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=NGDR-se, ymax=NGDR+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))









# plot everything (using standard error and not standard deviation)
#ggplot(data=vsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Diluted Cells, Day 3", x = "Condition", y = "Mean Velocity (um/sec)")) +
  scale_fill_manual("Cond",values = c("#CCCCCC", "#000000"))+theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none") 

#ggplot(data=freqsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Diluted Cells, Day 3", x = "Condition", y = "Turning Rate")) +
  scale_fill_manual("Cond",values = c("#CCCCCC", "#000000"))+theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")

cond=c ('Alox', 'Control')
mean=c(with(test, tapply(freq, cond, mean))) 
se=c(by (test$freq, test$cond, std.error))
sd=c(with(test, tapply(freq, cond, sd)))
freqsum<- data.frame(cond, mean, se, sd)

#normality test
by(test$Vm, test$cond, shapiro.test) 
by(test$freq, test$cond, shapiro.test)
shapiro.test(test$Vm)
shapiro.test(test$freq)
#If p values from normality tests are all greater 0.05 then it is acceptable. 

##histogram checking
ggplot(data = test[test$cond == "con",] , aes(x = Vm))+ geom_histogram(binwidth = diff(range(test$Vm))/10)+ 
  facet_wrap(~cond, scale="free")

##histogram checking
ggplot(data = test[test$cond == "alox",] , aes(x = Vm))+ geom_histogram(binwidth = diff(range(test$Vm))/10)+ 
  facet_wrap(~cond, scale="free")

qplot(Vm ~ cond, data = test)

#homogeneity of variance
#  the null hypothesis is that all populations variances are equal; 
# the alternative hypothesis is that at least two of them differ.
qplot(cond, Vm, data =test,  geom = "boxplot")
bartlett.test(Vm ~ cond, data=test)
library(lawstat)
levene.test(test$Vm, group=test$cond, location="mean")
levene.test(test$freq, group=test$cond, location="mean")


#non-parametric (use wilcoxon test if group=2, kruskal wallis if group>2)
wilcox.test(Vm~cond, data=test, paired=FALSE)
wilcox.test(freq~cond, data=test, paired=FALSE)
#If p values are below 0.05, data are significant from each other


#correlation between velocity and turning freq
cor.test(test$Vm, test$freq, method="kendall")
#If p value is below 0.05, data are significantly correlated to each other

summary(Kendall (test$Vm, test$freq))

qplot(Vm, freq, data=test) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(title="Correlation of Mean Velocity and Turning Rate", x = "Mean Velocity", y = "Turning Rate"))+ 
  theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")
