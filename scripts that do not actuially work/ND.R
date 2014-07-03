##Rename Data
t1<- trackdata2

## GrossDistance (GD)
GD <- aggregate( V ~ A , data = t1 , sum , na.rm = TRUE )

## Computing the NetDistance (ND)
## Split the data
dfs <- split(t1,t1$A)

## calculation
NDtemp1 <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )
                   [c(NA,2:nrow(x))-1], simplify = TRUE, USE.NAMES = TRUE)

## Convert to usable data and append to dataset
ND<-unsplit(NDtemp1, t1$A)
NM1<-cbind(t1, ND)

## NetDistanceSquared (ND2)
ND2=ND*ND
newmatrix2<-cbind(NM1, ND2)

## Export completed dataset
write.csv2(newmatrix2, "d:/Karen's/PhD/R/Processed_data/newmatrix.csv")

plot(0)
title(main="DO NOT FORGET TO RENAME THE FILE!!!", col.main="red")
print("DO NOT FORGET TO RENAME THE FILE!!!")

