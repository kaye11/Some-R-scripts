##binning by time. use spatially binned data 
##using the saved binned data, compute parameters by time range

tm=seq(0, 600, by = 30)
si.binned$time <- cut(si.binned$T, tm, include.lowest=T, labels=paste(head(tm, -1L), tail (tm, -1L), sep="-"))
con.binned$time <- cut(con.binned$T, tm, include.lowest=T, labels=paste(head(tm, -1L), tail (tm, -1L), sep="-"))

si.binned$cond="Si"
con.binned$cond="Con"
#si.binned$angle=NULL
#si.binned$angle2=NULL
#si.binned$scalar=NULL
#con.binned$angle=NULL
#con.binned$angle2=NULL
#con.binned$scalar=NULL

VN<- readline("What data did you analyse? all data! binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(si.binned, Vid, sep=";", col.names=T, row.names=F)

VN<- readline("What data did you analyse? without outbin! data?binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(con.binned, Vid, sep=";", col.names=T, row.names=F)

all.binned=rbind (si.binned, con.binned)

VN<- readline("What data did you analyse? without outbin! data?binned temporal?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(all.binned, Vid, sep=";", col.names=T, row.names=F)

dfc <- summarySE(all.binned, measurevar="V", na.rm=TRUE, groupvars=c("cond","bin", "time"))



all.binned$time=as.numeric(all.binned$time)

## GAM models for Bins

BinA<-gam(V ~  s(time,by = as.numeric(cond == "Con")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binA",])  
BinB<-gam(V ~  s(time,by = as.numeric(cond == "Con")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binB",])  
BinC<-gam(V ~  s(time,by = as.numeric(cond == "Con")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binC",])  
outbin<-gam(V ~  s(time,by = as.numeric(cond == "Con")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "outbin",])  


summary(BinA)
summary(BinB)
summary(BinC)
summary(outbin)

#whole dataset GAMs

all.binned$cb <- paste(all.binned$cond, all.binned$bin, sep = "")

BinAll<-gam(V ~  s(time,by = as.numeric(cb == "SibinA")) +
            s(time,by = as.numeric(cb == "SibinB")) + 
            s(time,by = as.numeric(cb == "SibinC")) +
            s(time,by = as.numeric(cb == "Sioutbin")) +
            s(time,by = as.numeric(cb == "ConbinA")) + 
            s(time,by = as.numeric(cb == "ConbinB"))+ 
            s(time,by = as.numeric(cb == "ConbinC")) + 
              s(time,by = as.numeric(cb == "Conoutbin")),         
          data = all.binned)  

notbinned<-gam(V ~  s(time,by = as.numeric(cond == "Con")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned) 


##plot whole data set

grid.newpage()
text <- element_text(face = "bold", size = 15) #change the size of the axes
theme_set(theme_bw())

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin A"
    value[value=="binB"]   <- "Bin B"
    value[value=="binC"] <- "Bin C"
    value[value=="outbin"]   <- "Outside bins"
  }
  return(value)
}


ggplot(data=all.binned, aes (x=T, y=V, group=cond, color=cond)) + geom_smooth(method="loess", size=1) + 
  labs(title="Mean Velocity (Binned)", x="Time", y="Mean Velocity (µm/sec)")+ facet_grid (.~bin, labeller=mf_labeller)+
  theme(axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =25, face="bold"), 
        legend.title = element_text(colour="black", size=15, face="bold"), 
        legend.text = element_text(colour="black", size = 15, face = "bold"), 
        axis.text=text, strip.text.x = element_text(size = 15, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ 
  scale_colour_discrete(name  ="Treatment", labels=c("Control", "Silica"))


ggplot(data=all.binned, aes (x=T, y=V, group=cond, color=cond)) + geom_smooth(method="loess", size=1) + 
  labs(title="Mean Velocity", x="Time", y="Mean Velocity (µm/sec)")+ 
  theme(axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =25, face="bold"), 
        legend.title = element_text(colour="black", size=15, face="bold"), 
        legend.text = element_text(colour="black", size = 15, face = "bold"), 
        axis.text=text, strip.text.x = element_text(size = 15, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ 
  scale_colour_discrete(name  ="Treatment", labels=c("Control", "Silica"))

