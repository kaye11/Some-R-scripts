# This makes a unique track identifier
NM$TID = as.factor(paste(NM$ID, NM$A, sep = "t"))
NM$A = as.factor(NM$A)
# how many pixels do you have per micrometer
pixperum = 20
# speed in um
NM$Vcor = NM$V/pixperum
# from mm per 1/25th per second to um per s
NM$VcorNasPs = NM$Vcor * 25
# Net distance travelled in um
NM$ND = NM$ND/pixperum
# Net distance squared
NM$ND.2 = NM$ND^2
# Time T in seconds
NM$T = NM$T/25

# Total Number of tracks
length(unique(NM$TID))

# Mean swimming speeds
mean(NM$VcorNasPs, na.rm = TRUE)

# make the mean ND2 for each timestep over all tracks
Rdata3 = aggregate(NM$ND.2, by = list(time = NM$T), mean, na.rm =
                     T)
Rdata3$RMS = sqrt(Rdata3$x)

# Taylor equation
m4 = nls(RMS ~ ((v^2) * tau * 2 * (time - tau * (1 -exp(-(time/tau)))))^0.5,
         data = Rdata3, start = list(v = 5, tau = 0.6), lower = 0.01,
         algorithm = "port")
coef(m4)

distance = coef(m4)[1] * coef(m4)[2]
decordistance = coef(m4)[1] * coef(m4)[2]
v = coef(m4)[1]
tau = coef(m4)[2]

# RMS distance travelled
Rdata3$MF = (((v^2) * tau * 2) * (Rdata3$time - tau * (1 -exp(-(Rdata3$time/tau)))))^0.5
qplot(time, MF, data = Rdata3)

##Plotting trajectories
qplot(X, Y, data = NM, color = A, group = A)
qplot(X, Y, data = NM) + facet_wrap(~A)