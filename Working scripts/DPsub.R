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

##For subsetting data
SD <-do.call(rbind, lapply(split(NM, NM$TID), head, 80))
SD$T=NULL
SD$T=(1:300)*0.10
SD1 <- readline("What subset data is this?")
SD2 <- paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/",SD1,".csv")
write.table(SD, SD2, sep=";", row.names = F)

# Total Number of tracks
lengthtracks=length(unique(SD$TID))

# Mean swimming speeds
Vcor=mean(SD$V, na.rm = TRUE)

# make the mean ND2 for each timestep over all tracks
Rdata3 = aggregate(SD$ND2, by = list(time = SD$T), mean, na.action = na.omit)
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
D = (((coef(m4)[1])^2) * (coef(m4)[2]))/2

## Export calculated parameters
TA2<- readline("What data did you analyse?")
TA<-cbind(TA2, lengthtracks, Vcor, v, tau, decordistance, D)
write.table(TA, "d:/Karen's/PhD/R program/Processed_data/trackanalysis.csv", 
            sep=";", col.names=F ,row.names = F, append=T)

## Summary
print(c(lengthtracks, Vcor, v, tau, decordistance, D))





