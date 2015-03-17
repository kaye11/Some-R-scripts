#This script is for checking pop based data of lifephase videos (day3 alox vs. control only)

library(ggplot2)
library(data.table)
library(gdata)
library(reshape)
library(plotrix)
library(Kendall)

##for control data
NT = data.table(c1, key="A")

con <- NT[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V), 
                    NGDR=mean(NGDR, na.rm=TRUE)), by=c("A")] 

con[,freq:=(ang/len),by=A]
con[,cond:=c("con"),by=A]

#for Si bead data

NT = data.table(s1, key="A")

si <- NT[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V), 
                  NGDR=mean(NGDR, na.rm=TRUE)), by=c("A")] 

si[,freq:=(ang/len),by=A]
si[,cond:=c("Si"),by=A]

#combining the two data sets
test=rbind(con, si)

##get standard error and mean Vm
cond=c ('Control', 'Si')
mean=c(with(test, tapply(Vm, cond, mean)))
sd=c(with(test, tapply(Vm, cond, sd)))
se=c(by (test$Vm, test$cond, std.error))
vsum<- data.frame(cond, mean, se, sd)

##get standard error and mean freq
cond=c ('Control', 'Si')
mean=c(with(test, tapply(freq, cond, mean)))
sd=c(with(test, tapply(freq, cond, sd)))
se=c(by (test$freq, test$cond, std.error))
freqsum<- data.frame(cond, mean, se, sd)

# plot everything (using standard error and not standard deviation)
ggplot(data=vsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Mean Speed of Starved Cells", x = "Condition", y = "Mean speed (µm/sec)")) +
  scale_fill_manual("Cond",values = c("#CCCCCC", "#000000"))+theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none") 

ggplot(data=freqsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Mean Turning Rate of Starved Cells", x = "Condition", y = "Turning Rate")) +
  scale_fill_manual("Cond",values = c("#CCCCCC", "#000000"))+theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")


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
ggplot(data = test[test$cond == "Si",] , aes(x = Vm))+ geom_histogram(binwidth = diff(range(test$Vm))/10)+ 
  facet_wrap(~cond, scale="free")

qplot(Vm ~ cond, data = test)

#homogeneity of variance
#  the null hypothesis is that all populations variances are equal; 
# the alternative hypothesis is that at least two of them differ.
qplot(cond, Vm, data =test,  geom = "boxplot")
bartlett.test(Vm ~ cond, data=test) #for normal data set
library(lawstat)
levene.test(test$Vm, group=test$cond, location="mean")
levene.test(test$freq, group=test$cond, location="mean")


#non-parametric (use wilcoxon test if group=2, kruskal wallis if group>2, paired=FALSE means you are using mann-whitney)
wilcox.test(Vm~cond, data=test, paired=FALSE)
wilcox.test(freq~cond, data=test, paired=FALSE)
#If p values are below 0.05, data are significant from each other


#correlation between velocity and turning freq
cor.test(test$Vm, test$freq, method="kendall")
#If p value is below 0.05, data are significantly correlated to each other

summary(Kendall (test$Vm, test$freq))

Kendall(ns$Vm, ns$freq)
Kendall(si$Vm, si$freq)
Kendall(con$Vm, con$freq)

summary(Kendall (si$Vm, si$freq))
summary(Kendall (con$Vm, con$freq))

qplot(Vm, freq, data=test) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(title="Correlation of Mean Velocity and Turning Rate", x = "Mean Velocity", y = "Turning Rate"))+ 
  theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")


qplot(Vm, freq, data=si) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(title="Correlation of Mean Velocity and Turning Rate \n(Silica)", x = "Mean Velocity", y = "Turning Rate"))+ 
  theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")

qplot(Vm, freq, data=con) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(title="Correlation of Mean Velocity and Turning Rate \n(Control)", x = "Mean Velocity", y = "Turning Rate"))+ 
  theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")


