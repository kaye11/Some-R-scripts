## Split the data
dfs <- split(t1,t1$A)

## Compute net distance
sim <- lapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )[1:nrow(x)] )
sim2 <- lapply( 1:length(results) , function(x) assign( paste0( "df." , unique(t1$A)[x] ) , results[[x]] ) )

results[[ND]]
results[[1]]

lapply( 1:length(results) , function(x) assign( paste0( "df." , unique(t1$A)[x] ) , results[[x]] ) )

jor <- lapply( dfs , function(x) as.matrix(dist( x[,c("X","Y")] , diag = TRUE )) )

tr