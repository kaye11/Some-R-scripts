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
