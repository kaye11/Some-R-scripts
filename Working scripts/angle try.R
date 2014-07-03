x<-c(0, 1, 3, 4, 4, 4, 4)
y<-c(0, 1, 1, 0, 2, 4, -1)

k=as.data.frame(cbind(x,y))

a=k[2:(nrow(k)-1),1]
b=k[2:(nrow(k)-1),2]
c=k[1:(nrow(k)-2),1]
d=k[1:(nrow(k)-2),2]
e=k[3:nrow(k),1]
f=k[3:nrow(k),2]

deg=180/pi
angle<-acos(((a-c)*(a-e)+(b-d)*(b-f))/(sqrt((a-c)^2+(b-d)^2)*sqrt((a-e)^2+(b-f)^2)))*deg


x<-c(0,1,3,3,4,4,4,4)
y<-c(0,1,1,1,0,2,4,-1)

getAngle <- function(X, Y) acos( sum(X*Y) / ( sqrt(sum(X * X)) * sqrt(sum(Y * Y)) ) )
lapply(split(t1[, c("X", "Y")], f = list(t1$A)), 
       FUN = function(x){ getAngle(x[, 1], x[, 2])})


df <- rbind(df,df,df)     # replicate the original data 3 times
df$K <- rep(1:3,each=6)   # K = 1, 2, 3
# theta in degrees
theta <- function(a,b)(180/pi)*(acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)))))
# this returns a vector of the angles between successive line segmeents
get.angles <- function(df.split){
  dx<- diff(df.split$X)
  dy<- diff(df.split$Y)
  sapply(1:(nrow(df.split)-2),function(i){
    a <- c(dx[i],dy[i])
    b <- c(dx[i+1],dy[i+1])
    theta(a,b)
  }) 
}
# this calls get.angles(...) for each subset of df, based on K
sapply(split(df,df$K),get.angles)