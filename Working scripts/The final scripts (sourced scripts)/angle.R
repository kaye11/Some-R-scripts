#function for computing angle
theta <- function(x,Y) apply(Y,1,function(x,y) acos((x%*%y) / 
                                                       ( sqrt(sum(x^2)) * sqrt(sum(y^2)) ) ),x=x)

##What is the xy position of the bead in this video
angStr <- readline("Enter xy position of bead: ");
a <- as.numeric(unlist(strsplit(angStr, ",")));

a
b <- t1[, c("X","Y")]
t1$thetarad=theta(a,b)
t1$thetadeg=theta(a,b)*57.2957795

