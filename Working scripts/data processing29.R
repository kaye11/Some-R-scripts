## Loading packages
library(ggplot2)
library(multcomp)
library(nlme)
library(lme4)
library(gdata)
library(grid)

"%!in%" <- function(x, y) !(x %in% y)

## Computing the NetDistance (ND) and GrossDistance (GD)
## Split the data
dfs <- split(t1,t1$A)

## calculation of GD
GDtemp <- sapply( dfs , function(x) cumsum( x[,c("V")]) [1:nrow(x)])

## calculation of ND
NDtemp1 <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )
                   [c(NA,2:nrow(x))-1], simplify = TRUE, USE.NAMES = TRUE)

## Convert to usable data and append to dataset
ND<-unsplit(NDtemp1, t1$A)
GD<-unsplit(GDtemp, t1$A)

##Calculation of ND2
ND2=ND*ND

##Binding data
NM<-cbind(t1,GD,ND,ND2)

# This makes a unique track identifier
NM$TID = as.factor(paste(NM$ID, NM$A, sep = "t"))
NM$A = as.factor(NM$A)
# from um per 1/25th per second to um per s
NM$VcorNasPs = NM$V * 25

# Total Number of tracks
lengthtracks=length(unique(NM$TID))

# Mean swimming speeds
Vcor=mean(NM$VcorNasPs, na.rm = TRUE)

# make the mean ND2 for each timestep over all tracks
Rdata3 = aggregate(NM$ND2, by = list(time = NM$T), mean, na.action = na.omit)
Rdata3$RMS = sqrt(Rdata3$x)

# Taylor equation
m4 = nls(RMS ~ ((v^2) * tau * 2 * (time - tau * (1 -
                                                   exp(-(time/tau)))))^0.5,
         data = Rdata3, start = list(v = 5, tau = 0.6), lower = 0.01,
         algorithm = "port", nls.control(maxiter=1000))
coef(m4)

##Computing decorrelation length scale
decordistance = coef(m4)[1] * coef(m4)[2]
v = coef(m4)[1]
tau = coef(m4)[2]

##Computing diffusivity (D)
D = (((coef(m4)[1])^2) * (coef(m4)[2]))/3

## Export completed dataset
NM2<- readline("What name should the file have?")
NM3<- paste("d:/Karen's/PhD/R/Processed_data/",NM2,".csv")
write.table(NM, NM3, sep=";", row.names = F)

## Export calculated parameters
TA2<- readline("What data did you analyse?")
TA<-cbind(TA2, lengthtracks, Vcor, v, tau, decordistance, D)
write.table(TA, "d:/Karen's/PhD/R/Processed_data/trackanalysis.csv", sep=";", col.names=F ,row.names = F, append=T)

## Summary
print(c(lengthtracks, Vcor, v, tau, decordistance, D))
