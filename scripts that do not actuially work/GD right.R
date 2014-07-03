GD <- aggregate( V ~ A , data = t1 , sum , na.rm = TRUE )

## Split the data
dfs <- split(t1,t1$A)

## Find hypotenuse between first and last rows for each A
lapply( dfs , function(x){
  j <- nrow(x)
  str <- x[1,c("X","Y")]
  end <- x[j,c("X","Y")]
  ND <- sqrt( sum( (end - str)^2 ) )
  return((ND))
} )

ND <- lapply( dfs , function(x) dist( x[,3:4] , diag = TRUE) )
NDtry2 <- lapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )[1:nrow(x)] )

attach (GD, ND)

ls()return(list(v1=GD,v2=ND))
ND2 <- ND^2
