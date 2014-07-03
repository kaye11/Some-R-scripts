##descriptives
library(psych)
library(ggplot)

#gives n, mean, sd, median, trimmed mean, mad(median abs deviation), min, max, range, skew, kurtosis,se
describeBy(aloxsum, aloxsum$cond)
describeBy(aloxsum, aloxsum$bin)
describe(aloxsum)

#check histogram
#general one
ggplot(data = aloxsum, aes(x = meanvs))+ geom_histogram(binwidth = diff(range(aloxsum$meanvm))/10)+ 
  facet_wrap(~cond, scale="free")

op=par(mfrow=c(2,2), mar=c(4,4,4,4))
hist(aloxsum$meanvs, prob=TRUE, main="Histogram of observed data") #getting a histogram with kernel density
lines(density(aloxsum$meanvs, na.rm = T, from = 0, to = max(aloxsum$meanvs)))
plot(density(aloxsum$meanvs),main="Density estimate of data") #getting a density plot
plot(ecdf(aloxsum$meanvs),main= "Empirical cumulative distribution function")
z.norm<-(aloxsum$meanvs-mean(aloxsum$meanvs))/sd(aloxsum$meanvs) ## standardized data 
qqnorm(z.norm) ## drawing the QQplot 
abline(0,1) ## drawing a 45-degree reference line 


