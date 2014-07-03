##THIS CODE ONLY WORKS WITH DATA FRAMES AND NOT WITH DATA TABLES! binning spatially

t1=as.data.frame(t1)

##BINNING
library(plotrix)
library(MASS)
library(data.table)
library(fBasics)
library(TeachingDemos)
library(data.table)
library(mgcv)

##What is the xy position of the bead in this video
angStr <- readline("Enter xy position of bead: ");
a <- as.numeric(unlist(strsplit(angStr, ",")));

a
center <- a
radii <- seq(112, 336, 112)


# calculate the distance to the center for each row of object t1, apply does not work with data.tables
distcenter <- apply(t1[, c("X", "Y")], 1, function(rowi) dist(rbind(rowi, center)))
# assign each row of object t1 to a group based on distcenter
group <- cut(distcenter, c(-1, radii, 2*max(radii)), labels=FALSE)

# draw circles in x,y coordinate space, use the eqscplot() function from package MASS
eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(t1$X, t1$Y, col=group)

# to subset into bins
binA=t1[group==1, ]
binB=t1[group==2, ]
binC=t1[group==3, ]
outbin=t1[group==4, ]

#t1
t1binA=binA
t1binB=binB
t1binC=binC
t1out=outbin

t1binA$bin=c("binA")
t1binB$bin=c("binB")
t1binC$bin=c("binC")
t1out$bin=c("outbin")

t1=rbind (t1binA, t1binB, t1binC, t1out)

VN<- readline("What data did you analyse? SPECIFY DATA TYPE:")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/",VN,".csv")
write.table(t1, Vid, sep=";", col.names=T, row.names=F)


##to count cells in each bin on a specific time and to save it on separate csv files
#BinA
count=sapply(c(0,seq(30,599,30),599),
             function(x)
               length(unique(binA[ binA$T==x, "A"])))

angle=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(binA[ binA$T==x, "angs"])))

speed=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(binA[ binA$T==x, "V"])))

time=(c(0,seq(30,599,30),599))
#Export and saving to a csv
binA<- readline("What data did you analyse? SPECIFY DATA AND BIN! BIN A!")
TA<-cbind(binA, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/binned exact/",binA,".csv"), 
            sep=";", row.names = F, na="NA", col.names=TRUE)

#BinB
count=sapply(c(0,seq(30,599,30),599),
             function(x)
               length(unique(binB[ binB$T==x, "A"])))

angle=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(binB[ binB$T==x, "angs"])))

speed=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(binB[ binB$T==x, "V"])))

time=(c(0,seq(30,599,30),599))
#Export and saving to a csv
binB<- readline("What data did you analyse? SPECIFY DATA AND BIN! BIN B!")
TA<-cbind(binB, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/binned exact/",binB,".csv"), 
            sep=";", row.names = F, na="NA")


#binC
count=sapply(c(0,seq(30,599,30),599),
             function(x)
               length(unique(binC[ binC$T==x, "A"])))

angle=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(binC[ binC$T==x, "angs"])))

speed=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(binC[ binC$T==x, "V"])))

time=(c(0,seq(30,599,30),599))
#Export and saving to a csv
binC<- readline("What data did you analyse? SPECIFY DATA AND BIN! BIN C!")
TA<-cbind(binC, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/binned exact/",binC,".csv"), 
            sep=";", row.names = F, na="NA")

#outbin
count=sapply(c(0,seq(30,599,30),599),
             function(x)
               length(unique(outbin[ outbin$T==x, "A"])))

angle=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(outbin[ outbin$T==x, "angs"])))

speed=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(outbin[ outbin$T==x, "V"])))

time=(c(0,seq(30,599,30),599))
#Export and saving to a csv
outbin<- readline("What data did you analyse? SPECIFY DATA AND BIN! OUTBIN!")
TA<-cbind(outbin, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/binned exact/",outbin,".csv"), 
            sep=";", row.names = F, na="NA")

##all cells as correction if needed
count=sapply(c(0,seq(30,599,30),599),
             function(x)
               length(unique(t1[ t1$T==x, "A"])))

angle=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(t1[ t1$T==x, "angs"])))

speed=sapply(c(0,seq(30,599,30),599),
             function(x)
               mean(unique(t1[ t1$T==x, "V"])))

time=(c(0,seq(30,599,30),599))
#Export and saving to a csv
all<- readline("What data did you analyse? SPECIFY DATA AND BIN! ALL CELLS!")
TA<-cbind(all, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/binned exact/",all,".csv"), 
            sep=";", row.names = F, na="NA")


