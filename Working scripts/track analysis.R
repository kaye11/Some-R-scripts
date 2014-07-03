# This makes a unique track identifier
NM$TID = as.factor(paste(NM$ID, NM$A, sep = "t"))
NM$A = as.factor(NM$A)
# from um per 1/25th per second to um per s
NM$VcorNasPs = NM$V * 1/25

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
decordistance

##Computing diffusivity (D)
D = (((coef(m4)[1])^2) * (coef(m4)[2]))/2
D

print(c(lengthtracks, Vcor, v, tau, decordistance, D))

# RMS distance travelled
Rdata3$MF = (((v^2) * tau * 2) * (Rdata3$time - tau * (1 -exp(-(Rdata3$time/tau)))))^0.5
RMS.plot=qplot(time, MF, data = Rdata3, na.rm=T)
  RMS.plot + labs(list(x = "Time (s)", y = "RMS (um)")) 
()

##Plotting trajectories (whole data set)
qplot(X, Y, data = NM, color = A, group = A)+ 
  geom_point (shape=24, size=2, fill="black")
qplot(X, Y, data = NM) + facet_wrap(~A) 

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







######WORKED BUT NOT REALLY THE WAY I WANT TO
##subsetting data for plots and plotting trajectories
NM$A <- as.numeric (as.character(NM$A))

#subset1
sub.data1<-subset(NM, A==20, select=c(A:TID))
qplot(X, Y, data = sub.data1, color = TID, group = TID)+ 
  geom_point (shape=16, size=3, fill="black")

sub.data1<-subset(NM, A<3, select=c(A:TID))
qplot(X, Y, data = sub.data1, color = TID, group = TID)+ 
  geom_line(linetype=5, size=1.5) + 
  geom_point (shape=19, size=5, fill="black")


s.traject((sub.data1[,c ("X","Y")]), fac=factor(sub.data1[,c ("A")]), 
        grid=T, addaxes=T, xlim=NULL, ylim=NULL, edge=T)

sub.data1<-subset(NM, A<5 , select=c(A:TID))
qplot(X, Y, data = sub.data1, color = TID, group = TID)+ 
  geom_line(linetype=1, size=1.5, )
+
  geom_path(size=2, linejoin="round", lineend="square", position="identity") +
  geom_point()


qplot(x=X, y=Y, data = sub.data1, color = TID, group = TID)+ 
  geom_line(linetype=0, size=1.5, arrow=arrow(angle=15, ends="both", type="closed"))+
  geom_point (shape=19, size=5, fill="black")

sub.data1<-subset(NM, A<2 , select=c(A:TID))
qplot(X, Y, data = sub.data1, color = TID, group = TID)+ 
  geom_path(linetype=0, size=1.5, arrow=arrow(angle=15, ends="both", type="closed"))+
  geom_point (shape=19, size=5, fill="black")

qplot(X, Y, data = NM, color = A, group = A)+ 
  geom_line(linetype=2, size=1.5) + 
  geom_point (shape=23, size=3, fill="black")
