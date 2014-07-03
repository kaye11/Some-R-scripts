#This script is for combining the two parts of mot-001 and combining at least 2 videos for 
#the one with alox

library(ggplot2)
library(data.table)
library(gdata)
library(reshape)
library(plotrix)
library(Kendall)

##for control data
NT = data.table(m1, key="A")
NT2= data.table(m2, key="A")

mot1.1 <- NT[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V), 
                    NGDR=mean(NGDR, na.rm=TRUE)), by=c("A")] 
mot1.2 <- NT2[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V), 
                     NGDR=mean(NGDR, na.rm=TRUE)), by=c("A")]

mot1.1[,freq:=(ang/len),by=A]
mot1.2[,freq:=(ang/len),by=A]
mot1.1[,cond:=c("con"),by=A]
mot1.2[,cond:=c("con"),by=A]

mot1=rbind(mot1.1, mot1.2)

#for diluted cells data

NT = data.table(d1, key="A")
NT2= data.table(d2, key="A")

dil1 <- NT[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V), 
                    NGDR=mean(NGDR, na.rm=TRUE)), by=c("A")] 
dil2 <- NT2[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V), 
                     NGDR=mean(NGDR, na.rm=TRUE)), by=c("A")]

dil1[,freq:=(ang/len),by=A]
dil2[,freq:=(ang/len),by=A]
dil1[,cond:=c("alox"),by=A]
dil2[,cond:=c("alox"),by=A]

dil=rbind(dil1, dil2)


#combining the two data sets
test=rbind(mot1, dil2)

##get standard error and meatest
cond=c ('Alox', 'Control')
mean=c(with(test, tapply(Vm, cond, mean)))
sd=c(with(test, tapply(Vm, cond, sd)))
se=c(by (test$Vm, test$cond, std.error))
vsum<- data.frame(cond, mean, se, sd)


# plot everything (using standard error and not standard deviation)
ggplot(data=vsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Diluted Cells, Day 3", x = "Condition", y = "Mean Velocity (um/sec)")) +
  scale_fill_manual("Cond",values = c("#CCCCCC", "#000000"))+theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none") 

ggplot(data=freqsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
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
