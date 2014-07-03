## Computing the NetDistance (ND) and GrossDistance (GD)
## Split the data
dfs <- split(t1,t1$A)

##GD and ND for each track

#Gross distance computation
GDR= aggregate( V ~ A , data = t1 , sum , na.rm = TRUE )


#Net distance computation
NDR=as.data.frame(lapply( dfs , function(x){
   j <- nrow(x)
   str <- x[1,c("X","Y")]
   end <- x[j,c("X","Y")]
   dist <- sqrt( sum( (end - str)^2 ) )
   return( dist )
 } ))

NDR2=t(NDR)

dis=cbind(GDR,NDR2)

dis$NGDR=dis$NDR2/dis$V
ave=mean(dis$NGDR)