library(ggplot2)
library(data.table)
library(gdata)
library(reshape)
library(plotrix)
library(Kendall)
library(ggthemes)
library(gridExtra)

nor=n1
sta=s1
#subsetting data to 30s
n1 <-ddply(nor, .(A), head, n = 30)
s1 <-ddply(sta, .(A), head, n = 30)

##Plotting trajectories (whole data set) + guides show the legend in an order (t1)
qplot(X, Y, data = n1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse() + geom_path(aes(group=factor(A)))

qplot(X, Y, data = s1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse() + geom_path(aes(group=factor(A)))

## for both control and treatment
#normal(n1)
NT = data.table(n1)
NT2=NT[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
NTS <- aggregate( A ~ T , data = NT2 , max, na.rm = TRUE )
NTS=rename(NTS, c(A="Norm"))


#s1(starving)
CN = data.table(s1)
CN2=CN[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
CNS <- aggregate( A ~ T , data = CN2, max, na.rm = TRUE )
CNS=rename(CNS, c(A="St"))
CNS$T=NULL

#data binding (Si=A, Con=Con)
CB=cbindX(NTS, CNS)

##for checking RMS

ggplot(CB, aes(T)) + geom_line(aes(y = Norm, colour = "Normal"),  size=1) + 
  geom_line(aes(y = St, colour = "Starving"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks", title="Motility Day 3")) + 
  theme(legend.title=element_blank())


##frequency of track change (angle change)
norm <- NT[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V)), by=c("A")] 
star <- CN[, list(ang=length(unique(angle)), len=length(T), Vm=mean(V)), by=c("A")]

norm$freq=norm$ang/norm$len
star$freq=star$ang/star$len
norm$cond=c("norm")
star$cond=c("star")

ns=rbind(norm, star)

##get standard error and means
cond=c ('Normal', 'Starving')
mean=c(with(ns, tapply(Vm, cond, mean)))
sd=c(with(ns, tapply(Vm, cond, sd)))
se=c(by (ns$Vm, ns$cond, std.error))
vsum<- data.frame(cond, mean, se, sd)

cond=c ('Normal', 'Starving')
mean=c(with(ns, tapply(freq, cond, mean))) 
se=c(by (ns$freq, ns$cond, std.error))
sd=c(with(ns, tapply(freq, cond, sd)))
freqsum<- data.frame(cond, mean, se, sd)

# plot everything (using standard error and not standard deviation)
ggplot(data=vsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Mean Speed", x = "Condition", y = "Mean speed (um/sec)")) +
  scale_fill_manual("Cond",values = c("grey70", "black"))+theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none") 

ggplot(data=freqsum, aes(x=cond, y=mean, width=0.75)) + geom_bar(aes(fill = cond), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(list(title="Day 3 Starved, Turning Rate", x = "Condition", y = "Turning Rate")) +
  scale_fill_manual("Cond",values = c("#CCCCCC", "#000000"))+theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")

##t-test
#boxplot
qplot(cond, Vm, color = cond, data = ns,  geom = "boxplot")
qplot(cond, freq, color = cond, data = ns,  geom = "boxplot")

#normality test
by(ns$Vm, ns$cond, shapiro.test) 
by(ns$freq, ns$cond, shapiro.test) 
shapiro.test(ns$Vm)
shapiro.test(ns$freq)
#If p values from normality tests are all greater 0.05 then it is acceptable. 

#homogeneity of variance
#  the null hypothesis is that all populations variances are equal; 
# the alternative hypothesis is that at least two of them differ.
# use bartlett test if data is normally distributed, otherwise use Levene
qplot(cond, Vm, data =ns,  geom = "boxplot")

library(lawstat)
levene.test(ns$Vm, group=ns$cond, location="mean")
levene.test(ns$freq, group=ns$cond, location="mean")
#If p values from the tests are all greater 0.05 then it is acceptable. 

#non-parametric (use wilcoxon test if group=2, kruskal wallis if group>2)
wilcox.test(Vm~cond, data=ns, paired=FALSE)
wilcox.test(freq~cond, data=ns, paired=FALSE)
#If p values are below 0.05, data are significant from each other

#correlation between velocity and turning freq
cor.test(ns$Vm, ns$freq, method="kendall")
#If p value is below 0.05, data are significantly correlated to each other

summary(Kendall (ns$Vm, ns$freq))

qplot(Vm, freq, data=ns) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(title="Correlation of Mean Velocity and Turning Rate", x = "Mean Velocity", y = "Turning Rate"))+ 
  theme_classic()+
  theme(axis.text=element_text(size=25, face="bold"), axis.title=element_text(size=25,face="bold"), 
        plot.title = element_text(size =35, face="bold"), legend.position="none")

##correlation by condition

cor.test(~ Vm + freq, data=ns, subset=cond=="norm", method="kendall") 
qplot(Vm, freq, data=ns [cond=="norm"]) + stat_smooth(method="lm", se=TRUE, size=1)+
  labs(list(x = "Mean Velocity", y = "Turning Rate"))

cor.test(~ Vm + freq, data=ns, subset=cond=="star", method="kendall") 
qplot(Vm, freq, data=ns [cond=="norm"]) + stat_smooth(method="lm", se=TRUE, size=1, na.action=na.omit)+
  labs(list(x = "Mean Velocity", y = "Turning Rate"))


##plotting general

grid.newpage()
text <- element_text(size = 30) #change the size of the axes
theme_set(theme_bw())

library(plyr)
vsum$cond2=revalue(cond, c("Normal"="Normal medium", "Starving"="Si-free medium"))

ggplot(data=vsum, aes(x=cond2, y=mean, width=0.75)) + geom_bar(aes(fill = cond2), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(y = "Mean Speed (µm/s)") +
  scale_fill_manual("Cond",values = c("black", "darkred"))+
  theme(axis.text.y=element_text(size=30), axis.title.y=element_text(size=30,face="bold", vjust=-0.05), 
        plot.title = element_text(size =30, face="bold"), axis.title.x = element_blank(), 
                                  axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,2), "cm"), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +  
  scale_y_continuous(expand=c(0,0)) 

##plotting ppt

grid.newpage()
text <- element_text(size = 30) #change the size of the axes
theme_set(theme_bw())

library(plyr)
vsum$cond2=revalue(cond, c("Normal"="+Si", "Starving"="-Si"))

ggplot(data=vsum, aes(x=cond2, y=mean, width=0.75)) + geom_bar(aes(fill = cond2), position = "dodge", stat="identity") +
  geom_errorbar(width=.1, size=1, aes(ymin=mean-se, ymax=mean+se))+
  labs(y = "Mean Speed (µm/s)") +
  scale_fill_manual("Cond",values = c("darkred", "black"))+
  theme(axis.text.y=element_text(size=30), axis.title.y=element_text(size=30,face="bold", vjust=-0.05), 
        plot.title = element_text(size =30, face="bold"), axis.title.x = element_blank(), 
        axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,2), "cm"), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +  
  scale_y_continuous(expand=c(0,0)) 

