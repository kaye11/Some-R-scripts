theta <- function(a,b)(180/pi)*(acos(sum(a*b)/(sqrt(sum(a*a))*sqrt(sum(b*b)))))
get.angles <- function(df.split){
  dx<- diff(df.split$X)
  dy<- diff(df.split$Y)
  stops <- which(dx^2+dy^2==0)
  dx<- dx[-stops]
  dy<- dy[-stops]
  df<- df.split[-(stops+1),]
  sapply(1:(length(dx)-1),function(i){
    a <- c(dx[i],dy[i])
    b <- c(dx[i+1],dy[i+1])
    return(cbind(df[i+1,],angle=180-theta(a,b)))
  })
}
result <- t(do.call(cbind,lapply(split(df,df$A),get.angles)))
result


r=merge(df,result[, c (1:2, 7)], by=c("A", "T"), all=TRUE)

result <- t(do.call(cbind, lapply(split(df,df$A),get.angles)))