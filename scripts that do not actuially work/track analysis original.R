#Changing directory
getwd()
setwd("D:\\Karen's\\PhD\\R program")
getwd()

##Rename Data
newmatrix<- trackdata

#Installing packages

library(ggplot2)
library(multcomp)
library(nlme)
library(lme4)
library(nlme)
library(lme4)
"%!in%" <- function(x, y) !(x %in% y)

# This makes a unique track identifier
newmatrix$TID = as.factor(paste(newmatrix$ID, newmatrix$A, sep = "t"))
newmatrix$A = as.factor(newmatrix$A)
# how many pixels do you have per micrometer
pixperum = 20
# speed in um
newmatrix$Vcor = newmatrix$V/pixperum
# from mm per 1/25th per second to um per s
newmatrix$VcorNasPs = newmatrix$Vcor * 25
# Net distance travelled in um
newmatrix$ND = newmatrix$ND/pixperum
# Net distance squared
newmatrix$ND.2 = newmatrix$ND^2
# Time T in seconds
newmatrix$T = newmatrix$T/25

# Total Number of tracks
length(unique(newmatrix$TID))

# Mean swimming speeds
mean(newmatrix$VcorNasPs, na.rm = TRUE)

# make the mean ND2 for each timestep over all tracks
Rdata3 = aggregate(newmatrix$ND.2, by = list(time = newmatrix$T), mean, na.rm =
                     T)
Rdata3$RMS = sqrt(Rdata3$x)

# Taylor equation
m4 = nls(RMS ~ ((v^2) * tau * 2 * (time - tau * (1 -
                                                   exp(-(time/tau)))))^0.5,
         data = Rdata3, start = list(v = 5, tau = 0.6), lower = 0.01,
         algorithm = "port")
coef(m4)