GD <- aggregate( V ~ A , data = t1 , sum , na.rm = TRUE )

## Split the data
dfs <- split(t1,t1$A)

## Compute net distance
sim3 <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )[1:nrow(x)] )
sim2=as.matrix(sim)

jor <- lapply( dfs , function(x) as.matrix(dist( x[,c("X","Y")] , diag = TRUE )[1:nrow(x)]) )

jor <- lapply( dfs , function(x) as.matrix(dist( x[,c("X","Y")] , diag = TRUE ) ))

sim4 <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )[1:nrow(x)], simplify = TRUE, USE.NAMES = TRUE )

sim5 <- sapply( dfs , function(x) dist( x[,c("X","Y")] , diag = TRUE )
                [1:nrow(x)-1], simplify = TRUE, USE.NAMES = TRUE )

sim6 <- sapply( dfs , function(x) as.matrix(dist( x[,c("X","Y")] , diag = TRUE ), ncol=1
                [1:nrow(x)-1], simplify = TRUE, USE.NAMES = TRUE ))
as