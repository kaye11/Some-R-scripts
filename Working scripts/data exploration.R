library(ggplot2)
library(fmsb)
library(data.table)
library(plyr)

aloxsum <- ddply(alox, .(A, bin, cond, time), summarise, meanvm = mean(Vm, na.rm = TRUE), 
                 meanfreq = mean(freq, na.rm = TRUE), meandir=mean(dir, na.rm=TRUE), meanvs=mean(sqrt(Vm)))
aloxsum2<- ddply(alox, .(A, bin, cond), summarise, meanvm = mean(Vm, na.rm = TRUE), 
                            meanfreq = mean(freq, na.rm = TRUE), meandir=mean(dir, na.rm=TRUE), meanvs=mean(sqrt(Vm)))

aloxsum$T <- ts(aloxsum$time) #coerce time to become a ts object
str(aloxsum)
qplot(bin, meanvs, data = aloxsum, geom = "boxplot", fill = cond) + theme_classic()

##transform categorical variables to numeric character
aloxsum$condn=as.numeric(aloxsum$cond)
aloxsum$binn=as.numeric(aloxsum$bin)

##checking for outliers using boxplot
qplot(bin, meanvs, data = aloxsum, geom = "boxplot", fill = cond) + facet_grid(~cond, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Velocity (um/sec)"))
#or
qplot(cond, meanvs, data = aloxsum, geom = "boxplot", fill = cond) + facet_grid(~bin, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Velocity (um/sec)"))

qplot(cond, freq, color = cond, data = alox,  geom = "boxplot") + 
  facet_wrap (~bin+T, scales="free") +
  labs(list(title="Si vs. control bead velocity", x = "Condition", y = "Turning rate"))

##cleveland plot
dotchart (aloxsum$meanvs, groups=factor(aloxsum$condn), ylab="Condition", 
          xlab="Mean Vs", main="Cleveland plot", pch=aloxsum$condn)

#cleveland plot on a subset of data
nn = unique(aloxsum$A)
nn2=sample(nn, 100)
ns1=aloxsum[aloxsum$A %in% nn2,]

dotchart (ns1$meanvs, groups=factor(ns1$condn), ylab="Condition", 
          xlab="Mean Vs", main="Cleveland plot", pch=ns1$condn)

##used to detect relationships between variables and collinearity
#all
pr=data.table(cbind(cond=aloxsum$condn, bin=aloxsum$binn,T=aloxsum$T, 
                    vs=aloxsum$meanvs, freq=aloxsum$meanfreq, dir=aloxsum$meandir))
pairs(pr)
#explanatory variables
pairs(exp)
#response variables
resp=data.table(cbind(vs=aloxsum$meanvs, freq=aloxsum$meanfreq, dir=aloxsum$meandir))
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
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meanvs)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meanvs)
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meanfreq)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meanfreq)
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meandir)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meandir)
par(op)


op=par(mfrow=c(3,3), mar=c(5,4,2,2))
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meanvs)
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meanfreq)
interaction.plot(aloxsum$T, aloxsum$cond, aloxsum$meandir)

interaction.plot(aloxsum$T, aloxsum$bin, aloxsum$meanvs)
interaction.plot(aloxsum$T, aloxsum$bin, aloxsum$meanfreq)
interaction.plot(aloxsum$T, aloxsum$bin, aloxsum$meandir)

interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meanvs)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meanfreq)
interaction.plot(aloxsum$bin, aloxsum$cond, aloxsum$meandir)


par(op)

##histogram checking
ggplot(data = aloxsum[aloxsum$bin == "binA",] , aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvs))/10)+ 
  facet_wrap(~cond, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binA",] , aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvs))/10)+ 
  facet_wrap(~cond*T, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binB",] , aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvs))/10)+ 
  facet_wrap(~cond, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binB",] , aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvs))/10)+ 
  facet_wrap(~cond*T, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binC",] , aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvs))/10)+ 
  facet_wrap(~cond, scale="free")

ggplot(data = aloxsum[aloxsum$bin == "binC",] , aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvs))/10)+ 
  facet_wrap(~cond*T, scale="free")

#general one
ggplot(data = aloxsum, aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvs))/10)+ 
  facet_wrap(cond~bin, scale="free")

#normality check
by(aloxsum$meanvs [aloxsum$bin=="binA"], aloxsum$cond [aloxsum$bin=="binA"], shapiro.test)
by(aloxsum$meanvs [aloxsum$bin=="binB"], aloxsum$cond [aloxsum$bin=="binB"], shapiro.test)
by(aloxsum$meanvs [aloxsum$bin=="binC"], aloxsum$cond [aloxsum$bin=="binC"], shapiro.test)

#try a model, do not transform yet
m0=lm(meanvs~cond+bin+T, data=aloxsum) #m0=lm(meanvs~cond+bin+T, data=aloxsum)
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

M1 <- gls(meanvs~bin*cond, data=aloxsum, correlation = corCompSymm(form =~T/A))


bartlett.test(e, aloxsum$cond)
bartlett.test(e, aloxsum$bin)
bartlett.test(e, aloxsum$T)

m1=lme(meanvs~cond/bin*cond/T*bin/T, data=aloxsum, random=~1|T)

# Combine phase and treatment
aloxsum$cb <- paste(aloxsum$cond, aloxsum$bin, sep = "")

## GAM models for Bins

m2<-gam(meanvs ~s(T,by = as.numeric(cb == "ContbinA")) +
          s(T,by = as.numeric(cb == "ContbinB")) + 
          s(T,by = as.numeric(cb == "ContbinC")) +
          s(T,by = as.numeric(cb == "SibinA")) +
          s(T,by = as.numeric(cb == "SibinB")) + 
          s(T,by = as.numeric(cb == "SibinC")), 
        data = aloxsum)  

m3<-gam(meanvs ~  cond+s(T,by = as.numeric(bin == "binA")) + 
          s(T,by = as.numeric(bin == "binB")) + 
          s(T,by = as.numeric(bin == "binC")), 
        data = aloxsum)

m4a<-gam(meanvs ~  s(T,by = as.numeric(cond == "Cont")) + 
          s(T,by = as.numeric(cond == "Si")), 
        data = aloxsum [aloxsum$bin=="binA", ])

m4b<-gam(meanvs ~  s(T,by = as.numeric(cond == "Cont")) + 
           s(T,by = as.numeric(cond == "Si")), 
         data = aloxsum [aloxsum$bin=="binB", ])

m4c<-gam(meanvs ~  s(T,by = as.numeric(cond == "Cont")) + 
           s(T,by = as.numeric(cond == "Si")), 
         data = aloxsum [aloxsum$bin=="binC", ])


m5a<-gam(meanvm ~  s(T,by = as.numeric(cond == "Cont")) + 
           s(T,by = as.numeric(cond == "Si")), 
         data = aloxsum [aloxsum$bin=="binA", ])

m5b<-gam(meanvm ~  s(T,by = as.numeric(cond == "Cont")) + 
           s(T,by = as.numeric(cond == "Si")), 
         data = aloxsum [aloxsum$bin=="binB", ])

m5c<-gam(meanvm ~  s(T,by = as.numeric(cond == "Cont")) + 
           s(T,by = as.numeric(cond == "Si")), 
         data = aloxsum [aloxsum$bin=="binC", ])