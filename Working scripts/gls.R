library(nlme)
library(ggplot2)

#load alox2 data
alox=alox2
alox$Vs=sqrt(alox$Vm) #sqrt transformation
alox$T <- ts(alox$time) #coerce time to become a ts object

#plot data
qplot(time, Vs, data=alox)+facet_wrap(~cond+bin)

#initial model without correlation
M0=gls(meanvm~bin+cond+T, data=alox)
summary(M0)
M0res=residuals(M0, type="normalized")
plot(M0res)
acf(M0res)
##Residuals usually are theoretically assumed to have an ACF that has 
#correlation = 0 for all lags.

#compound symmetry auto-correlation structure
M1 <- gls(Vs~bin+cond+T, data=alox, correlation = corCompSymm(form =~T))



fm <- lme(Vs~cond*bin+T, random = ~1|A, 
          correlation=corAR1(0.5, form=~1|A/T), data=alox) 
