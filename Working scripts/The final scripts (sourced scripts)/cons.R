##THIS CODE IS TO MAKE A TABLE WITH SPECIFIC TIME POINTS, NUMBER OF CELLS, 
#SPEED OF EACH CELL, AND ANGLE OF EACH CELL! THE CODE FOR ANGLE DETERMINATION
#IS ALREADY INCLUDED IN THIS CODE. JUST SOURCE IT.

#function for computing angle
theta <- function(x,Y) apply(Y,1,function(x,y) acos((x%*%y) / 
                                                      ( sqrt(sum(x^2)) * sqrt(sum(y^2)) ) ),x=x)

##What is the xy position of the bead in this video
angStr <- readline("Enter xy position of bead: ");
a <- as.numeric(unlist(strsplit(angStr, ",")));

a
b <- t1[, c("X","Y")]
t1$thetarad=theta(a,b)
t1$thetadeg=theta(a,b)*57.2957795

##BINNING
library(plotrix)
library(MASS)

# define the center and radii of the circles
center <- c(455, 351)
radii <- seq(112, 336, 112)

# calculate the distance to the center for each row of object t1
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

##to count cells in each bin on a specific time and to save it on separate csv files
#BinA
count=sapply(c(1,seq(30,598,30),598),
       function(x)
         length(unique(binA[ binA$T==x, "A"])))

angle=sapply(c(1,seq(30,598,30),598),
       function(x)
         mean(unique(binA[ binA$T==x, "thetadeg"])))

speed=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(binA[ binA$T==x, "V"])))

time=(c(1,seq(30,598,30),598))
#Export and saving to a csv
binA<- readline("What data did you analyse? SPECIFY DATA AND BIN! BIN A!")
TA<-cbind(binA, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/points/",binA,".csv"), 
            sep=";", row.names = F, na="NA")

#BinB
count=sapply(c(1,seq(30,598,30),598),
             function(x)
               length(unique(binB[ binB$T==x, "A"])))

angle=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(binB[ binB$T==x, "thetadeg"])))

speed=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(binB[ binB$T==x, "V"])))

time=(c(1,seq(30,598,30),598))
#Export and saving to a csv
binB<- readline("What data did you analyse? SPECIFY DATA AND BIN! BIN B!")
TA<-cbind(binB, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/points/",binB,".csv"), 
            sep=";", row.names = F, na="NA")


#binC
count=sapply(c(1,seq(30,598,30),598),
             function(x)
               length(unique(binC[ binC$T==x, "A"])))

angle=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(binC[ binC$T==x, "thetadeg"])))

speed=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(binC[ binC$T==x, "V"])))

time=(c(1,seq(30,598,30),598))
#Export and saving to a csv
binC<- readline("What data did you analyse? SPECIFY DATA AND BIN! BIN C!")
TA<-cbind(binC, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/points/",binC,".csv"), 
            sep=";", row.names = F, na="NA")

#outbin
count=sapply(c(1,seq(30,598,30),598),
             function(x)
               length(unique(outbin[ outbin$T==x, "A"])))

angle=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(outbin[ outbin$T==x, "thetadeg"])))

speed=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(outbin[ outbin$T==x, "V"])))

time=(c(1,seq(30,598,30),598))
#Export and saving to a csv
outbin<- readline("What data did you analyse? SPECIFY DATA AND BIN! OUTBIN!")
TA<-cbind(outbin, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/points/",outbin,".csv"), 
            sep=";", row.names = F, na="NA")

##all cells as correction if needed
count=sapply(c(1,seq(30,598,30),598),
             function(x)
               length(unique(t1[ t1$T==x, "A"])))

angle=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(t1[ t1$T==x, "thetadeg"])))

speed=sapply(c(1,seq(30,598,30),598),
             function(x)
               mean(unique(t1[ t1$T==x, "V"])))

time=(c(1,seq(30,598,30),598))
#Export and saving to a csv
all<- readline("What data did you analyse? SPECIFY DATA AND BIN! ALL CELLS!")
TA<-cbind(all, time, count, speed, angle)
write.table(TA, paste ("d:/Karen's/PhD/R program/Processed_data/points/",all,".csv"), 
            sep=";", row.names = F, na="NA")


