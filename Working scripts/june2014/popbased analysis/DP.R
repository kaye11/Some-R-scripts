## Loading packages
library(ggplot2)
library(multcomp)
library(nlme)
library(lme4)
library(gdata)
library(grid)
library(plyr)

"%!in%" <- function(x, y) !(x %in% y)

# This makes a unique track identifier
NM$A = as.factor(NM$A)

##For subsetting data! CHANGE the mins subset!
SD <- subset (NM, NM$T<601) #8 mins
#SD <- subset (NM, NM$T>481 & NM$T<600) #8mins
#SD <- subset (NM, NM$T>300) #10mins


SD1 <- readline("What subset data is this?")
write.table(SD, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/",SD1,".csv"), sep=";", row.names = F)

NM=SD

# Total Number of tracks
lengthtracks=length(unique(NM$A))

# Mean swimming speeds
Vcor=mean(NM$V, na.rm = TRUE)
NM$ND2=NM$ND*NM$ND

# make the mean ND2 for each timestep over all tracks
Rdata3 = aggregate(NM$ND2, by = list(time = NM$T), mean, na.action = na.omit)
Rdata3$RMS = sqrt(Rdata3$x)


# Taylor equation, fitting the curve
m4 = nls(RMS ~ ((v^2) * tau * 2 * (time - tau * (1 -
                                                   exp(-(time/tau)))))^0.5,
         data = Rdata3, start = list(v = 5, tau = 0.6), lower = 0.01,
         algorithm = "port", nls.control(maxiter=1000))
coef(m4)


##Computing decorrelation length scale
decordistance = coef(m4)[1] * coef(m4)[2]
v = coef(m4)[1]
tau = coef(m4)[2]

##Computing diffusivity (D) (divisor is equal to 2 if video is 2d, 3 if video is 3d)
D = (((coef(m4)[1])^2) * (coef(m4)[2]))/2

Rdata3$MF = (((v^2) * tau * 2) * (Rdata3$time - tau * (1 -exp(-(Rdata3$time/tau)))))^0.5

## Export completed dataset
NM2<- readline("What name should the file have? This is your processed data:")
NM3<- paste("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/parameters/",NM2,".csv")
write.table(NM, NM3, sep=";", row.names = F)

## Export calculated parameters
TA2<- readline("What data did you analyse? This is the summary for trackanalysis data:")
TA<-cbind(TA2, lengthtracks, Vcor, v, tau, decordistance, D)
write.table(TA, "d:/Karen's/PhD/R program/Processed_data/trackanalysis.csv", 
            sep=";", col.names=F ,row.names = F, append=T)

##Export Rdata3
RM2<- readline("What RMS data is this?")
RM3<- paste("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/parameters/RMS/",RM2,".csv")
write.table(Rdata3, RM3, sep=";", row.names = F)

## Summary
print(c(lengthtracks, Vcor, v, tau, decordistance, D))


#Simple RMS plotting with the axis titles changed
Rdata3$MF = (((v^2) * tau * 2) * (Rdata3$time - tau * (1 -exp(-(Rdata3$time/tau)))))^0.5
RMS.plot=qplot(time, MF, data = Rdata3, na.rm=T)
RMS.plot + labs(list(x = "Time (s)", y = "RMS (µm)"))


# RMS distance travelled, computation and plotting RMS with t, regression line computed and 
#overlayed with data points
Rdata3$MF = (((v^2) * tau * 2) * (Rdata3$time - tau * (1 -exp(-(Rdata3$time/tau)))))^0.5
RMS.plot=qplot(time, MF, data = Rdata3, na.rm=T) + geom_point (shape=21, size=3, color="black")
newtime<- seq(from=min(Rdata3[,1]), to=max(Rdata3[,1]), length=length(Rdata3[,1])) 
reglineY <- predict(m4,list(time = newtime)) 
RMS.plot + labs(list(x = "Time (s)", y = "RMS (µm)")) + 
  geom_line(aes(x=newtime, y= reglineY), colour = "red", lwd=2)


