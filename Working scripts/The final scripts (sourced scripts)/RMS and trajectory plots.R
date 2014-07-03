# RMS distance travelled, computation and plotting RMS with t
Rdata3$MF = (((v^2) * tau * 2) * (Rdata3$time - tau * (1 -exp(-(Rdata3$time/tau)))))^0.5
RMS.plot=qplot(time, MF, data = Rdata3, na.rm=T) + geom_point (shape=21, size=3, color="black")
newtime<- seq(from=min(Rdata3[,1]), to=max(Rdata3[,1]), length=length(Rdata3[,1])) 
reglineY <- predict(m4,list(time = newtime)) 
RMS.plot + labs(list(x = "Time (s)", y = "RMS (µm)")) + 
  geom_line(aes(x=newtime, y= reglineY), colour = "red", lwd=2)


##Plotting trajectories (whole data set) + guides show the legend in an order
all=qplot(X, Y, data = t1, color = factor(A), group = factor(A)) 
all=guides(col = guide_legend(nrow = 25)) 


qplot(X, Y, data = t1, color=V) + facet_wrap(~A) + scale_colour_gradientn(colours=heat.colors(12)) 

##Plotting trajectories (whole data set)
qplot(X, Y, data = NM, color = A, group = A)+ 
  geom_point (shape=24, size=1, fill="black")
qplot(X, Y, data = NM, color=V) + facet_wrap(~A) + scale_colour_gradientn(colours=heat.colors(12)) 

qplot(X, Y, data = NM, color = factor(A), group = factor(A)) 

##subsetting data for plots and plotting trajectories
NM$A <- as.numeric (as.character(NM$A))

#subset1
sub.data1<-subset(NM, A<21, select=c(A:TID))
qplot(X, Y, data = sub.data1, color = TID, group = TID)+ 
  geom_point (shape=24, size=2, fill="black")
qplot(X, Y, data = sub.data1) + facet_wrap(~A)
#subset2
sub.data2<-subset(NM, A>20 & A<41, select=c(A:TID))
qplot(X, Y, data = sub.data2, color = TID, group = TID)+ geom_line(size=1.5)
qplot(X, Y, data = sub.data2) + facet_wrap(~A) + geom_line(size=1.5)
#subset3
sub.data3<-subset(NM, A>40 & A<61, select=c(A:TID))
qplot(X, Y, data = sub.data3, color = TID, group = TID)+ geom_line(size=1.5)
qplot(X, Y, data = sub.data3) + facet_wrap(~A) + geom_line(size=1.5)
#subset4
sub.data4<-subset(NM, A>60 & A<81, select=c(A:TID))
qplot(X, Y, data = sub.data4, color = TID, group = TID)+ geom_line(size=1.5)
qplot(X, Y, data = sub.data4) + facet_wrap(~A) + geom_line(size=1.5)
#subset5
sub.data5<-subset(NM, A>80 & A<101, select=c(A:TID))
qplot(X, Y, data = sub.data5, color = TID, group = TID)+ geom_line(size=1.5)
qplot(X, Y, data = sub.data5) + facet_wrap(~A) + geom_line(size=1.5)

sub.data1<-sapply(NM, function(x) (NM[1:80, ]))
GDtemp <- sapply( dfs , function(x) cumsum( x[,c("V")]) [1:nrow(x)])
NDtemp1 <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )
                                     [c(NA,2:nrow(x))-1], simplify = TRUE, USE.NAMES = TRUE)

##For subsetting data
sub.data1=ddply(NM, .(TID), head, n = 80)
write.table(subdata1, "d:/Karen's/PhD/R/Processed_data/subdata1.csv", sep=";", row.names = F)


#subset1
sub.data1<-subset(NM, A<51, select=c(A:TID))
qplot(X, Y, data = sub.data1, color = TID, group = TID)
qplot(X, Y, data = sub.data1) + facet_wrap(~A) + scale_colour_gradientn(colours=heat.colors(12))
#subset2
sub.data2<-subset(NM, A>51, select=c(A:TID))
qplot(X, Y, data = sub.data2, color = TID, group = TID)+ geom_line(size=1.5)
qplot(X, Y, data = sub.data2) + facet_wrap(~A) + geom_line(size=1.5) + scale_colour_gradientn(colours=heat.colors(12))

qplot(X, Y, data=t1 [t1$A<51, ], color = factor(A), group = factor(A))+ guides (col = guide_legend(nrow = 25))
ggplot(data=t1 [t1$A<51, ], aes(x = X, y = Y, color = factor(A), group = factor(A))) + 
  geom_point() + geom_line()