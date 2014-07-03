## Setup
library(geosphere)
TimeInterval <- 0.04
pts <- t1 [c("X", "Y")]

## Pass in two derived data.frames that are lagged by one point
segDists <- dist2Line (p1 = pts[-nrow(t),], 
                 p2 = pts[-1,])
sum(GD)*TimeInterval
