#Changing directory
getwd()
setwd("D:\\Karen's\\PhD\\R program")
getwd()

##Rename Data
t1<- trackdata2

## GrossDistance (GD)
GD <- aggregate( V ~ A , data = t1 , sum , na.rm = TRUE )

## Computing the NetDistance (ND)
## Split the data
dfs <- split(t1,t1$A)

## calculation
NDtemp1 <- lapply( dfs , function(x) dist( x[,c("X","Y")], method = "euclidean")
                   [1:nrow(x)] )

while (t1$T==0) { (t1$X0 <- t1$X) (t1$Y0 <- t1$Y) } 
else { t1$ND.2=((t1$X0-t1$X)^2)+(t1$Y0-t1$Y)^2) }

apply (t1, )
## Convert to usable data and append to dataset
NDtemp2=as.matrix(NDtemp1)
NDtemp3<-unsplit(NDtemp2, t1$A)

## Ignore warnings from Unsplit
ND=as.matrix(NDtemp3)
NM1<-cbind(t1, ND)

## NetDistanceSquared (ND^2)
ND2=ND*ND
newmatrix<-cbind(NM1, ND2)

## Export completed dataset
write.table(newmatrix, "d:/Karen's/PhD/R/Processed_data/newmatrix.txt", sep="\t")

plot(0)
title(main="DO NOT FORGET TO RENAME THE FILE!!!", col.main="red")
print("DO NOT FORGET TO RENAME THE FILE!!!")

