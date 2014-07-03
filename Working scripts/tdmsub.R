##random sampling from data set of tdm (n1=normal, s2=starving)
nn = unique(n1$A)
nn2=sample(nn, 100)
ns1=n1[n1$A %in% nn2,]

ss = unique(s2$A)
ss2=sample(ss, 100)
ss1=s2[s2$A %in% ss2,]

library(ggplot2)
library(data.table)
library(gdata)
library(reshape)
library(plotrix)
library(Kendall)

##Plotting trajectories (whole data set) + guides show the legend in an order
qplot(X, Y, data = ns1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse() + geom_path(aes(group=factor(A)))

qplot(X, Y, data = ss1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse() + geom_path(aes(group=factor(A)))

## for both control and treatment
#normal(ns1)
NP = data.table(ns1)
NP2=NP[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
NPS <- aggregate( A ~ T , data = NP2 , max, na.rm = TRUE )
NPS=rename(NPS, c(A="Norm"))


#s2(ss1)
CP = data.table(ss1)
CP2=CP[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
CPS <- aggregate( A ~ T , data = CP2, max, na.rm = TRUE )
CPS=rename(CPS, c(A="St"))
CPS$T=NULL

#data binding (Si=A, Con=Con)
CBS=cbindX(NPS, CPS)

##for checking RMS

ggplot(CBS, aes(T)) + geom_line(aes(y = Norm, colour = "Normal"),  size=1) + 
  geom_line(aes(y = St, colour = "Starving"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks", title="Motility Day 3")) + 
  theme(legend.title=element_blank())


##frequency of track change (angle change)
normp <- NP[, list(ang=length(unique(P)), len=length(T), Vm=mean(V)), by=c("A")] 
starp <- CP[, list(ang=length(unique(P)), len=length(T), Vm=mean(V)), by=c("A")]

normp$freq=normp$ang/normp$len
starp$freq=starp$ang/starp$len
normp$cond=c("norm")
starp$cond=c("star")

nsp=rbind(normp, starp)

##get standard error and means
cond=c ('Normal', 'Starving')
mean=c(with(nsp, tapply(Vm, cond, mean))) 
se=c(by (nsp$Vm, nsp$cond, std.error))
sd=c(by (nsp$Vm, nsp$cond, sd))
vsump<- data.frame(cond, mean, se, sd)

cond=c ('Normal', 'Starving')
mean=c(with(nsp, tapply(freq, cond, mean))) 
se=c(by (nsp$freq, nsp$cond, std.error))
sd=c(with(nsp, tapply(freq, cond, sd)))
freqsump<- data.frame(cond, mean, se)

# plot everything (using standard error and not standard deviation)
ggplot(data=vsump, aes(cond, mean)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Day 3 Motility Mean Velocity Comparison", x = "Condition", y = "Mean Velocity (um/sec)"))

ggplot(data=freqsump, aes(cond, mean)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se)) +
  labs(list(title="Day 3 Motility Turning Frequency Comparison", x = "Condition", y = "Turning Rate"))

##t-test
#boxplot
qplot(cond, Vm, color = cond, data = nsp,  geom = "boxplot")
qplot(cond, freq, color = cond, data = nsp,  geom = "boxplot")

#normality test
by(nsp$Vm, nsp$cond, shapiro.test)
by(nsp$freq, nsp$cond, shapiro.test) 
#If p values from normality tests are all greater 0.05 then it is acceptable. 


#non-parametric (use wilcoxon test if group=2, kruskal wallis if group>2)
wilcox.test(Vm~cond, data=nsp)
wilcox.test(freq~cond, data=nsp)
#If p values are below 0.05, data are significant from each other


#correlation between velocity and turning freq
cor.test(nsp$Vm, nsp$freq, method="kendall")

#If p value is below 0.05, data are significantly correlated to each other

summary(Kendall (nsp$Vm, nsp$freq))

qplot(Vm, freq, data=nsp) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(x = "Mean Velocity", y = "Turning Rate"))

##correlation by condition

cor.test(~ Vm + freq, data=nsp, subset=cond=="norm", method="kendall") 
qplot(Vm, freq, data=nsp [cond=="norm"]) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(x = "Mean Velocity", y = "Turning Rate"))

cor.test(~ Vm + freq, data=nsp, subset=cond=="star", method="kendall") 
qplot(Vm, freq, data=nsp [cond=="norm"]) + stat_smooth(method="lm", se=TRUE, size=1, na.action=na.omit)+
  labs(list(x = "Mean Velocity", y = "Turning Rate"))
