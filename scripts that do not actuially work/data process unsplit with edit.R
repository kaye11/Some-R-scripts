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
NDtemp1 <- sapply( dfs , function(x) if ((x) == NA) {NA} else dist( x[,c("X","Y")] , diag = TRUE)  
                   [1:nrow(x)-1], simplify=TRUE, USE.NAMES=TRUE)

## Convert to usable data and append to dataset
NDtemp2=as.matrix(NDtemp1)
NDtemp3<-unsplit(NDtemp2, f=t1$A)

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
