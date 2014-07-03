## GrossDistance (GD)
GD <- aggregate( V ~ A , data = t1 , sum , na.rm = TRUE )




## Computing the NetDistance (ND)
## Split the data
dfs <- split(t1,t1$A)

## calculation
NDtemp <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE)
[c(NA,2:nrow(x))], simplify = TRUE, USE.NAMES = TRUE )

## Convert to usable data and append to dataset
ND<-unsplit(NDtemp, t1$A)
NM1<-cbind(t1, ND)

## NetDistanceSquared (ND^2)
ND2=ND*ND
newmatrix<-cbind(NM1, ND2)

## Export completed dataset
NM<- readline("What name should the file have?")
NM2<- paste("d:/Karen's/PhD/R/Processed_data/",NM,".txt")
write.table(newmatrix, NM2, , sep="\t")


