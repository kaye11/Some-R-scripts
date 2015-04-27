#ND

## Split the data
dfs <- split(binned,binned$ID)

NDtable <- NT[, list(str=max(GD), T=max(T)), by=c("ARF", "cond")] 


## Find hypotenuse between first and last rows for each A
NDtable2 <- as.data.frame(lapply( dfs , function(x){
  j <- nrow(x)
  str <- x[1,c("X","Y")]
  end <- x[j,c("X","Y")]
  dist <- sqrt( sum( (end - str)^2 ) )
  return( dist)
} ))

NDtable <- melt(NDtable2)

NDcond <- NT[, list(cond=cond, T=max(T)), by=c("ID", "cond")] 

NDtable3=cbind(NDtable, cond=NDcond$cond)

qplot(cond,value, data = NDtable3,  geom = "boxplot") + 
  labs(list(x = "Experimental Condition", y = "Net distance traveled (µm)"))

shapiro.test(NDtable3$value) #not normal
levene.test(NDtable3$value, group=NDtable3$cond, location="mean") #homogenous variances

t.test(value~cond, data=NDtable3) # no sig diff

wilcox.test(value~cond, data=NDtable3, paired=FALSE) #no sig diff


