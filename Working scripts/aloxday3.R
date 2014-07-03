library(ggplot2)
library(data.table)
library(gdata)
library(reshape)
library(plotrix)

##Plotting trajectories (whole data set) + guides show the legend in an order (t1)
qplot(X, Y, data = st1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse() + geom_path(aes(group=factor(A)))

qplot(X, Y, data = con1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse() + geom_path(aes(group=factor(A)))

## for both Si and control
#control (con1)
CT = data.table(con1)
CT2=CT[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
CT3 <- aggregate( A ~ T , data = CT2 , max, na.rm = TRUE )
Con=data.frame(T=c(1:598), bead=c("Con"), Tracks=CT3$A)


#Si
SI = data.table(st1)
SI2=SI[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
SIS1 <- aggregate( A ~ T , data = SI2, max, na.rm = TRUE )
SIS1=rename(SIS1, c(A="Si1"))
SIS1$T=NULL

SI = data.table(st2)
SI2=SI[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
SIS2 <- aggregate( A ~ T , data = SI2, max, na.rm = TRUE )
SIS2=rename(SIS2, c(A="Si2"))
SIS2$T=NULL

SI = data.table(st3)
SI2=SI[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
SIS3 <- aggregate( A ~ T , data = SI2, max, na.rm = TRUE )
SIS3=rename(SIS3, c(A="Si3"))
SIS3$T=NULL

SIS4=cbind(SIS1, SIS2, SIS3)
SIS4$all=rowSums (SIS4)

Si=data.frame(T=c(1:598),bead=c("Si"), Tracks=SIS4$all)

#data binding (Si=Si, Con=Con)
CD=rbind(Si, Con)


##for checking RMS
qplot(T,Tracks, color=bead, data = CD [CD$T<61, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="1Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>60 & CD$T<121, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="2Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>120 & CD$T<181, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="3Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>180 & CD$T<241, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="4Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>240 & CD$T<301, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="5Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>300 & CD$T<361, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="6Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>360 & CD$T<421, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="7Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>420 & CD$T<481, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="8Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>480 & CD$T<541, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="9Min")) + 
  theme(legend.title=element_blank())

qplot(T,Tracks, color=bead, data = CD [CD$T>540 & CD$T<601, ],  geom = "line") + geom_line(size=1) + 
  facet_wrap(~bead, scales="free")+ labs(list(x = "Time (s)", y = "Number of Tracks", 
                                              title="10Min")) + 
  theme(legend.title=element_blank())


