##mean and standard deviation
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
res <- as.data.frame(colMeans(norm))
res$normSd=colSd(norm)
res$starAve=colMeans(star)
res$starSd=colSd(star)
res$normcount=length(unique(n1$A))
res$starcount=length(unique(s2$A))