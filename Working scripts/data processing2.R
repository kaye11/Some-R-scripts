## Computing the NetDistance (ND) and GrossDistance (GD)
## Split the data
dfs <- split(t1,t1$A)

## calculation of GD
GDtemp <- sapply( dfs , function(x) cumsum( x[,c("V")]) [1:nrow(x)])

## calculation of ND
NDtemp1 <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )
                   [c(NA,2:nrow(x))-1], simplify = TRUE, USE.NAMES = TRUE)

## Convert to usable data and append to dataset
ND<-unsplit(NDtemp1, t1$A)
GD<-unsplit(GDtemp, t1$A)

##Calculation of ND2
ND2=ND*ND

##Binding data
NM<-cbind(t1,GD,ND,ND2)

## Export completed dataset
NM2<- readline("What name should the file have?")
NM3<- paste("d:/Karen's/PhD/R/Processed_data/",NM2,".csv")
write.table(NM, NM3, sep=";", row.names = F)
