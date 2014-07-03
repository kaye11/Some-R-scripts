library(ggplot2)
library(fmsb)

##transform categorical variables to numeric character
aloxsum$condn=as.numeric(aloxsum$cond)
aloxsum$binn=as.numeric(aloxsum$bin)

##checking for outliers using boxplot
qplot(bin, meanvm, data = aloxsum, geom = "boxplot", fill = cond) + facet_grid(~cond, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Velocity (um/sec)"))
#or
qplot(cond, meanvm, data = aloxsum, geom = "boxplot", fill = cond) + facet_grid(~bin, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Velocity (um/sec)"))

qplot(cond, freq, color = cond, data = alox,  geom = "boxplot") + 
  facet_wrap (~bin+T, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Turning rate"))

##transform data and check outliers again
aloxsum$meanvs=sqrt(aloxsum$meanvm)
qplot(cond, meanvs, data = aloxsum, geom = "boxplot", fill = cond) + facet_grid(~bin, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Velocity (um/sec)"))

##cleveland plot
dotchart (aloxsum$meanvm, groups=factor(aloxsum$condn), ylab="Condition", 
          xlab="Mean Vs", main="Cleveland plot", pch=aloxsum$condn)

#cleveland plot on a subset of data
nn = unique(aloxsum$A)
nn2=sample(nn, 100)
ns1=aloxsum[aloxsum$A %in% nn2,]

dotchart (ns1$meanvm, groups=factor(ns1$condn), ylab="Condition", 
          xlab="Mean Vs", main="Cleveland plot", pch=ns1$condn)

##used to detect relationships between variables and collinearity
#all
pr=data.table(cbind(cond=aloxsum$condn, bin=aloxsum$binn,T=aloxsum$T, 
                      vs=aloxsum$meanvs, freq=aloxsum$meanfreq, dir=aloxsum$meandir))
pairs(pr)
#explanatory variables
pairs(exp)
#response variables
resp=data.table(cbind(vs=aloxsum$meanvm, freq=aloxsum$meanfreq, dir=aloxsum$meandir))
pairs(resp)

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
exp=data.table(cbind(bin=aloxsum$binn, cond=aloxsum$condn, T=aloxsum$T))
cor(exp)
vif_func(in_frame=exp,thresh=5,trace=T)
#vif below zero indicates no collinearity, use all variables in the model
#no collinear and correlated, use all variables 

##checking relationships
op=par(mfrow=c(3,2), mar=c(5,4,2,2))
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meanvm)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meanvm)
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meanfreq)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meanfreq)
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meandir)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meandir)
par(op)

##histogram checking
ggplot(data = aloxsum[aloxsum$bin == "binA",] , aes(x = meanvm))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(~cond, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binA",] , aes(x = meanvm))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(~cond*T, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binB",] , aes(x = meanvm))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(~cond, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binB",] , aes(x = meanvm))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(~cond*T, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binC",] , aes(x = meanvm))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(~cond, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binC",] , aes(x = meanvm))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(~cond*T, scale="free")

#general one
ggplot(data = aloxsum, aes(x = meanvm))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(cond~bin, scale="free")

#normality check
by(aloxsum$meanvm [aloxsum$bin=="binA"], aloxsum$cond [aloxsum$bin=="binA"], shapiro.test)
by(aloxsum$meanvm [aloxsum$bin=="binB"], aloxsum$cond [aloxsum$bin=="binB"], shapiro.test)
by(aloxsum$meanvm [aloxsum$bin=="binC"], aloxsum$cond [aloxsum$bin=="binC"], shapiro.test)

#try a model, do not transform yet
m0=lm(meanvm~cond+bin+T, data=aloxsum) #m0=lm(meanvs~cond+bin+T, data=aloxsum)
drop1(m0, test="F") 
#check significance of interaction terms, pvalue lower, intercation is significant

#model validation
op=par(mfrow=c(2,3), mar=c(5,4,2,2))
plot(m0, add.smooth=FALSE, which=1)
e=resid(m0)
hist(e, xlab="Residuals", main="")
plot(aloxsum$cond, e, xlab="Mean Velocity", ylab="Residuals")
plot(aloxsum$bin, e, xlab="Mean Velocity", ylab="Residuals")
plot(aloxsum$T, e, xlab="Mean Velocity", ylab="Residuals")
par(op)

op=par(mfrow=c(2,2))
plot(m0, add.smooth=FALSE)

step(m0)


bartlett.test(e, aloxsum$cond)
bartlett.test(e, aloxsum$bin)
bartlett.test(e, aloxsum$T)

library(car)
leveneTest(count ~ spray, data=InsectSprays)
